use crate::equivalence::are_equivalent;
use crate::inline_snippets::inline_sorted_snippets_with_offset;
use crate::pattern::resolved_pattern::CodeRange;
use crate::pattern::state::{get_top_level_effects, FileRegistry};
use crate::pattern::{Effect, EffectKind};
use crate::smart_insert::calculate_padding;
use crate::suppress::is_suppress_comment;
use anyhow::{anyhow, Result};
use grit_util::AstNode;
use marzano_language::language::{FieldId, Language};
use marzano_language::target_language::TargetLanguage;
use marzano_util::analysis_logs::{AnalysisLogBuilder, AnalysisLogs};
use marzano_util::node_with_source::NodeWithSource;
use marzano_util::position::{Position, Range};
use marzano_util::tree_sitter_util::children_by_field_id_count;
use std::iter;
use std::ops::Range as StdRange;
use std::path::Path;
use std::{borrow::Cow, collections::HashMap, fmt::Display};
use tree_sitter::Node;

// the inner references hold the mutable state
#[derive(Debug, Clone)]
pub enum Constant {
    Boolean(bool),
    String(String),
    Integer(i64),
    Float(f64),
    Undefined,
}

impl Constant {
    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Constant::Integer(i) => *i != 0,
            Constant::Float(d) => *d != 0.0,
            Constant::Boolean(b) => *b,
            Constant::String(s) => !s.is_empty(),
            Constant::Undefined => false,
        }
    }

    pub(crate) fn is_undefined(&self) -> bool {
        matches!(self, Self::Undefined)
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Boolean(b) => write!(f, "{}", b),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Integer(n) => write!(f, "{}", n),
            Constant::Float(n) => write!(f, "{}", n),
            Constant::Undefined => write!(f, ""),
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constant::Boolean(b1), Constant::Boolean(b2)) => b1 == b2,
            (Constant::String(s1), Constant::String(s2)) => s1 == s2,
            (Constant::Integer(n1), Constant::Integer(n2)) => n1 == n2,
            (Constant::Float(n1), Constant::Float(n2)) => n1 == n2,
            _ => false,
        }
    }
}

pub trait Binding: Clone + std::fmt::Debug + PartialEq {
    fn get_sexp(&self) -> Option<String>;

    /// The range where the binding is found in the source text.
    fn position(&self) -> Option<Range>;

    /// Returns the text of the binding.
    fn text(&self) -> String;

    /// Returns a reference to the source text of the file in which the binding
    /// is found.
    fn source(&self) -> Option<&str>;

    /// Returns the path this binding applies to, if and only if this is a
    /// filename binding.
    fn as_filename(&self) -> Option<&Path>;

    /// Returns whether the binding is empty.
    fn is_empty(&self) -> bool;

    /// Tests equivalence between two bindings.
    ///
    /// Unlike equality, this tests whether two bindings appear to match the
    /// same thing.
    fn is_equivalent_to(&self, other: &Self) -> bool;

    fn is_suppressed(&self, lang: &impl Language, current_name: Option<&str>) -> bool;

    /// Logs an error about this binding if the given range is empty.
    fn log_empty_field_rewrite_error<T>(
        &self,
        // FIXME: This introduces a dependency on TreeSitter.
        language: &TargetLanguage,
        logs: &mut AnalysisLogs,
    ) -> Result<()>;

    /// Returns the padding to use for inserting the given text.
    fn get_insertion_padding(
        &self,
        text: &str,
        is_first: bool,
        language: &TargetLanguage,
    ) -> Option<String>;

    /// Returns the parent node of the binding.
    fn parent_node(&self) -> Option<impl AstNode>;
}

#[derive(Debug, Clone)]
// &str points to the file source
pub(crate) enum MarzanoBinding<'a> {
    // used by slices that don't correspond to a node
    // currently only comment content.
    String(&'a str, Range),
    FileName(&'a Path),
    Node(&'a str, Node<'a>),
    // tree-sitter lists ("multiple" fields of nodes) do not have a unique identity
    // so we represent them by the parent node and a field id
    List(&'a str, Node<'a>, FieldId),
    Empty(&'a str, Node<'a>, FieldId),
    ConstantRef(&'a Constant),
}

impl<'a> Binding for MarzanoBinding<'a> {
    fn get_sexp(&self) -> Option<String> {
        match self {
            Self::Node(_, node) => Some(node.to_sexp().to_string()),
            Self::List(_, parent_node, field_id) => {
                let mut cursor = parent_node.walk();
                let mut children = parent_node.children_by_field_id(*field_id, &mut cursor);
                let mut result = String::new();
                if let Some(node) = children.next() {
                    result.push_str(&node.to_sexp());
                    for node in children {
                        result.push_str(",\n");
                        result.push_str(&node.to_sexp());
                    }
                }
                Some(result)
            }
            Self::String(..) | Self::FileName(_) | Self::Empty(..) | Self::ConstantRef(_) => None,
        }
    }

    // todo implement for empty and empty list
    fn position(&self) -> Option<Range> {
        match self {
            Self::Empty(_, _, _) => None,
            Self::Node(_, node) => Some(Range::from(node.range())),
            Self::String(_, range) => Some(range.to_owned()),
            Self::List(_, parent_node, field_id) => {
                let mut cursor = parent_node.walk();
                let mut children = parent_node.children_by_field_id(*field_id, &mut cursor);

                match children.next() {
                    None => None,
                    Some(first_node) => {
                        let end_node: Node = match children.last() {
                            None => first_node.clone(),
                            Some(last_node) => last_node,
                        };
                        let mut leading_comment = first_node.clone();
                        while let Some(comment) = leading_comment.prev_sibling() {
                            if comment.kind() == "comment" {
                                leading_comment = comment;
                            } else {
                                break;
                            }
                        }
                        let mut trailing_comment = end_node;
                        while let Some(comment) = trailing_comment.next_sibling() {
                            if comment.kind() == "comment" {
                                trailing_comment = comment;
                            } else {
                                break;
                            }
                        }
                        Some(Range {
                            start: Position::new(
                                first_node.start_position().row() + 1,
                                first_node.start_position().column() + 1,
                            ),
                            end: Position::new(
                                trailing_comment.end_position().row() + 1,
                                trailing_comment.end_position().column() + 1,
                            ),
                            start_byte: leading_comment.start_byte(),
                            end_byte: trailing_comment.end_byte(),
                        })
                    }
                }
            }
            Self::FileName(_) => None,
            Self::ConstantRef(_) => None,
        }
    }

    fn text(&self) -> String {
        match self {
            Self::Empty(_, _, _) => String::new(),
            Self::Node(source, node) => {
                NodeWithSource::new(node.clone(), source).text().to_string()
            }
            Self::String(s, r) => s[r.start_byte as usize..r.end_byte as usize].into(),
            Self::FileName(s) => s.to_string_lossy().to_string(),
            Self::List(source, _, _) => {
                if let Some(pos) = self.position() {
                    source[pos.start_byte as usize..pos.end_byte as usize].to_string()
                } else {
                    String::new()
                }
            }
            Self::ConstantRef(c) => c.to_string(),
        }
    }

    fn source(&self) -> Option<&str> {
        match self {
            Self::Empty(source, _, _) => Some(source),
            Self::Node(source, _) => Some(source),
            Self::String(source, _) => Some(source),
            Self::List(source, _, _) => Some(source),
            Self::FileName(..) | Self::ConstantRef(..) => None,
        }
    }

    fn as_filename(&self) -> Option<&Path> {
        match self {
            Self::FileName(path) => Some(path),
            Self::Empty(..)
            | Self::Node(..)
            | Self::String(..)
            | Self::List(..)
            | Self::ConstantRef(..) => None,
        }
    }

    fn is_empty(&self) -> bool {
        matches!(self, Self::Empty(_, _, _))
    }

    fn is_equivalent_to(&self, other: &Self) -> bool {
        // covers Node, and List with one element
        if let (Some(binding1), Some(binding2)) = (self.singleton(), other.singleton()) {
            return are_equivalent(binding1.0, &binding1.1, binding2.0, &binding2.1);
        }

        match self {
            // should never occur covered by singleton
            Self::Node(source1, node1) => match other {
                Self::Node(source2, node2) => are_equivalent(source1, node1, source2, node2),
                Self::String(str, range) => {
                    str[range.start_byte as usize..range.end_byte as usize] == self.text()
                }
                Self::FileName(_) | Self::List(..) | Self::Empty(..) | Self::ConstantRef(_) => {
                    false
                }
            },
            Self::List(source1, parent_node1, field1) => match other {
                Self::List(source2, parent_node2, field2) => {
                    let mut cursor1 = parent_node1.walk();
                    let mut cursor2 = parent_node2.walk();
                    children_by_field_id_count(parent_node1, *field1)
                        == children_by_field_id_count(parent_node2, *field2)
                        && parent_node1
                            .children_by_field_id(*field1, &mut cursor1)
                            .zip(parent_node2.children_by_field_id(*field2, &mut cursor2))
                            .all(|(node1, node2)| are_equivalent(source1, &node1, source2, &node2))
                }
                Self::String(..)
                | Self::FileName(_)
                | Self::Node(..)
                | Self::Empty(..)
                | Self::ConstantRef(_) => false,
            },
            // I suspect matching kind is too strict
            Self::Empty(_, node1, field1) => match other {
                Self::Empty(_, node2, field2) => {
                    node1.kind_id() == node2.kind_id() && field1 == field2
                }
                Self::String(..)
                | Self::FileName(_)
                | Self::Node(..)
                | Self::List(..)
                | Self::ConstantRef(_) => false,
            },
            Self::ConstantRef(c1) => match other {
                Self::ConstantRef(c2) => c1 == c2,
                Self::String(..)
                | Self::FileName(_)
                | Self::Node(..)
                | Self::List(..)
                | Self::Empty(..) => false,
            },
            Self::String(s1, range) => {
                s1[range.start_byte as usize..range.end_byte as usize] == other.text()
            }
            Self::FileName(s1) => other.as_filename().map_or(false, |s2| *s1 == s2),
        }
    }

    fn get_insertion_padding(
        &self,
        text: &str,
        is_first: bool,
        language: &TargetLanguage,
    ) -> Option<String> {
        match self {
            Self::List(src, node, field_id) => {
                let mut cursor = node.walk();
                let children = node
                    .children_by_field_id(*field_id, &mut cursor)
                    .collect_vec();
                if children.is_empty() {
                    return None;
                }
                calculate_padding(src, &children, text, is_first, language).or_else(|| {
                    if children.len() == 1 {
                        let child = children.first().unwrap();
                        let child_text = child.utf8_text(src.as_bytes()).ok()?;
                        if child.end_position().row() > child.start_position().row()
                            && !child_text.ends_with('\n')
                            && !text.starts_with('\n')
                        {
                            return Some("\n".to_string());
                        }
                    }
                    None
                })
            }
            Self::Node(src, node) => {
                let node_text = node.utf8_text(src.as_bytes()).ok()?;
                if language.is_statement(node.kind_id())
                    && !node_text.ends_with('\n')
                    && !text.starts_with('\n')
                {
                    Some("\n".to_string())
                } else {
                    None
                }
            }
            Self::String(..) | Self::FileName(_) | Self::Empty(..) | Self::ConstantRef(_) => None,
        }
    }

    fn is_suppressed(&self, lang: &impl Language, current_name: Option<&str>) -> bool {
        let (src, node) = match self {
            Self::Node(src, node) | Self::List(src, node, _) | Self::Empty(src, node, _) => {
                (src, node)
            }
            Self::String(_, _) | Self::FileName(_) | Self::ConstantRef(_) => return false,
        };
        let target_range = node.range();
        for n in
            node.children(&mut node.walk())
                .chain(ParentTraverse::new(TreeSitterParentCursor::new(
                    node.clone(),
                )))
        {
            let mut cursor = n.walk();
            let children = n.children(&mut cursor);
            for c in children {
                if !(lang.is_comment(c.kind_id()) || lang.is_comment_wrapper(&c)) {
                    continue;
                }
                if is_suppress_comment(&c, src, &target_range, current_name, lang) {
                    return true;
                }
            }
        }

        false
    }
}

impl PartialEq for MarzanoBinding<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Empty(_, _, _), Self::Empty(_, _, _)) => true,
            (Self::Node(src1, n1), Self::Node(src2, n2)) => {
                n1.utf8_text(src1.as_bytes()) == n2.utf8_text(src2.as_bytes())
            }
            (Self::String(src1, r1), Self::String(src2, r2)) => {
                src1[r1.start_byte as usize..r1.end_byte as usize]
                    == src2[r2.start_byte as usize..r2.end_byte as usize]
            }
            (Self::List(_, n1, f1), Self::List(_, n2, f2)) => n1 == n2 && f1 == f2,
            (Self::ConstantRef(c1), Self::ConstantRef(c2)) => c1 == c2,
            _ => false,
        }
    }
}

fn pad_snippet(padding: &str, snippet: &str) -> String {
    // Write first snippet line as is, without extra padding
    let mut lines = snippet.split('\n');
    let mut result = lines.next().unwrap_or_default().to_string();

    // Add the rest of lines in the snippet with padding
    lines.for_each(|line| result.push_str(&format!("\n{}{}", &padding, line)));
    result
}

fn adjust_ranges(substitutions: &mut [(EffectRange, String)], index: usize, delta: isize) {
    for (EffectRange { range, .. }, _) in substitutions.iter_mut() {
        if range.start >= index {
            range.start = (range.start as isize + delta) as usize;
        }
        if range.end >= index {
            range.end = (range.end as isize + delta) as usize;
        }
    }
}

// in multiline snippets, remove padding from every line equal to the padding of the first line,
// such that the first line is left-aligned.
pub(crate) fn adjust_padding<'a>(
    src: &'a str,
    range: &CodeRange,
    new_padding: Option<usize>,
    offset: usize,
    substitutions: &mut [(EffectRange, String)],
) -> Result<Cow<'a, str>> {
    if let Some(new_padding) = new_padding {
        let newline_index = src[0..range.start as usize].rfind('\n');
        let pad_strip_amount = if let Some(index) = newline_index {
            src[index..range.start as usize]
                .chars()
                .take_while(|c| c.is_whitespace())
                .count()
                - 1
        } else {
            0
        };
        let mut result = String::new();
        let snippet = &src[range.start as usize..range.end as usize];
        let mut lines = snippet.split('\n');
        // assumes codebase uses spaces for indentation
        let delta: isize = (new_padding as isize) - (pad_strip_amount as isize);
        let padding = " ".repeat(pad_strip_amount);
        let new_padding = " ".repeat(new_padding);
        let mut index = offset;
        result.push_str(lines.next().unwrap_or_default());
        index += result.len();
        for line in lines {
            result.push('\n');
            index += 1;
            if line.trim().is_empty() {
                adjust_ranges(substitutions, index, -(line.len() as isize));
                continue;
            }
            adjust_ranges(substitutions, index, delta);
            let line = line.strip_prefix(&padding).ok_or_else(|| {
                anyhow!(
                    "expected line \n{}\n to start with {} spaces, code is either not indented with spaces, or does not consistently indent code blocks",
                    line,
                    pad_strip_amount
                )
            })?;
            result.push_str(&new_padding);
            index += new_padding.len();
            result.push_str(line);
            index += line.len();
        }
        for (_, snippet) in substitutions.iter_mut() {
            *snippet = pad_snippet(&new_padding, snippet);
        }
        Ok(result.into())
    } else {
        Ok(src[range.start as usize..range.end as usize].into())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct EffectRange {
    pub(crate) kind: EffectKind,
    pub(crate) range: StdRange<usize>,
}

impl EffectRange {
    pub(crate) fn new(kind: EffectKind, range: StdRange<usize>) -> Self {
        Self { kind, range }
    }

    pub(crate) fn start(&self) -> usize {
        self.range.start
    }

    // The range which is actually edited by this effect
    pub(crate) fn effective_range(&self) -> StdRange<usize> {
        match self.kind {
            EffectKind::Rewrite => self.range.clone(),
            EffectKind::Insert => self.range.end..self.range.end,
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn linearize_binding<'a, B: Binding>(
    language: &TargetLanguage,
    effects: &[Effect<'a, B>],
    files: &FileRegistry<'a>,
    memo: &mut HashMap<CodeRange, Option<String>>,
    source: &'a str,
    range: CodeRange,
    distributed_indent: Option<usize>,
    logs: &mut AnalysisLogs,
) -> Result<(Cow<'a, str>, Vec<StdRange<usize>>)> {
    let effects1 = get_top_level_effects(effects, memo, &range, language, logs)?;

    let effects1 = effects1
        .into_iter()
        .map(|effect| {
            let b = effect.binding;
            let (src, range) = match (b.source(), b.position()) {
                (Some(src), Some(orig_range)) => {
                    (Some(src), Some(CodeRange::from_range(src, orig_range)))
                }
                _ => {
                    b.log_empty_field_rewrite_error(language, logs)?;
                    (None, None)
                }
            };
            if let (Some(src), Some(range)) = (src, &range) {
                match effect.kind {
                    EffectKind::Rewrite => {
                        if let Some(o) = memo.get(range) {
                            if let Some(s) = o {
                                return Ok((b, s.to_owned().into(), effect.kind));
                            } else {
                                return Ok((
                                    b,
                                    adjust_padding(src, range, distributed_indent, 0, &mut [])?,
                                    effect.kind,
                                ));
                            }
                        } else {
                            memo.insert(range.clone(), None);
                        }
                    }
                    EffectKind::Insert => {}
                }
            }
            let res = effect.pattern.linearized_text(
                language,
                effects,
                files,
                memo,
                distributed_indent.is_some(),
                logs,
            )?;
            if let Some(range) = range {
                if matches!(effect.kind, EffectKind::Rewrite) {
                    memo.insert(range, Some(res.to_string()));
                }
            }
            Ok((b, res, effect.kind))
        })
        .collect::<Result<Vec<_>>>()?;

    let mut replacements: Vec<(EffectRange, String)> = effects1
        .iter()
        .map(|(b, s, k)| {
            let range = b
                .position()
                .ok_or_else(|| anyhow!("binding has no position"))?;
            match k {
                EffectKind::Insert => Ok((
                    EffectRange::new(
                        EffectKind::Insert,
                        range.start_byte as usize..range.end_byte as usize,
                    ),
                    s.to_string(),
                )),
                EffectKind::Rewrite => Ok((
                    EffectRange::new(
                        EffectKind::Rewrite,
                        range.start_byte as usize..range.end_byte as usize,
                    ),
                    s.to_string(),
                )),
            }
        })
        .collect::<Result<Vec<_>>>()?;

    // we need to update the ranges of the replacements to account for padding discrepency
    let adjusted_source = adjust_padding(
        source,
        &range,
        distributed_indent,
        range.start as usize,
        &mut replacements,
    )?;
    let (res, offset) = inline_sorted_snippets_with_offset(
        language,
        adjusted_source.to_string(),
        range.start as usize,
        &mut replacements,
        distributed_indent.is_some(),
    )?;
    memo.insert(range, Some(res.clone()));
    Ok((res.into(), offset))
}

impl<'a> MarzanoBinding<'a> {
    pub(crate) fn from_constant(constant: &'a Constant) -> Self {
        Self::ConstantRef(constant)
    }

    pub(crate) fn from_node(node: NodeWithSource<'a>) -> Self {
        Self::Node(node.source, node.node)
    }

    pub(crate) fn from_path(path: &'a Path) -> Self {
        Self::FileName(path)
    }

    pub(crate) fn from_range(range: Range, source: &'a str) -> Self {
        Self::String(source, range)
    }

    /// Returns the only node bound by this binding.
    ///
    /// This includes list bindings that only match a single child.
    ///
    /// Returns `None` if the binding has no associated node, or if there is
    /// more than one associated node.
    pub(crate) fn singleton(&self) -> Option<NodeWithSource<'a>> {
        match self {
            Self::Node(src, node) => Some(NodeWithSource::new(node.clone(), src)),
            Self::List(src, parent_node, field_id) => {
                let mut cursor = parent_node.walk();
                let mut children = parent_node.children_by_field_id(*field_id, &mut cursor);
                if let Some(node) = children.next() {
                    if children.next().is_none() {
                        Some(NodeWithSource::new(node.clone(), src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Self::String(..) | Self::FileName(..) | Self::Empty(..) | Self::ConstantRef(..) => None,
        }
    }

    pub fn get_sexp(&self) -> Option<String> {
        match self {
            Self::Node(_, node) => Some(node.to_sexp().to_string()),
            Self::List(_, parent_node, field_id) => {
                let mut cursor = parent_node.walk();
                let mut children = parent_node.children_by_field_id(*field_id, &mut cursor);
                let mut result = String::new();
                if let Some(node) = children.next() {
                    result.push_str(&node.to_sexp());
                    for node in children {
                        result.push_str(",\n");
                        result.push_str(&node.to_sexp());
                    }
                }
                Some(result)
            }
            Self::String(..) | Self::FileName(_) | Self::Empty(..) | Self::ConstantRef(_) => None,
        }
    }

    // todo implement for empty and empty list
    pub fn position(&self) -> Option<Range> {
        match self {
            Self::Empty(_, _, _) => None,
            Self::Node(_, node) => Some(Range::from(node.range())),
            Self::String(_, range) => Some(range.to_owned()),
            Self::List(_, parent_node, field_id) => {
                let mut cursor = parent_node.walk();
                let mut children = parent_node.children_by_field_id(*field_id, &mut cursor);

                match children.next() {
                    None => None,
                    Some(first_node) => {
                        let end_node: Node = match children.last() {
                            None => first_node.clone(),
                            Some(last_node) => last_node,
                        };
                        let mut leading_comment = first_node.clone();
                        while let Some(comment) = leading_comment.prev_sibling() {
                            if comment.kind() == "comment" {
                                leading_comment = comment;
                            } else {
                                break;
                            }
                        }
                        let mut trailing_comment = end_node;
                        while let Some(comment) = trailing_comment.next_sibling() {
                            if comment.kind() == "comment" {
                                trailing_comment = comment;
                            } else {
                                break;
                            }
                        }
                        Some(Range {
                            start: Position::new(
                                first_node.start_position().row() + 1,
                                first_node.start_position().column() + 1,
                            ),
                            end: Position::new(
                                trailing_comment.end_position().row() + 1,
                                trailing_comment.end_position().column() + 1,
                            ),
                            start_byte: leading_comment.start_byte(),
                            end_byte: trailing_comment.end_byte(),
                        })
                    }
                }
            }
            Self::FileName(_) => None,
            Self::ConstantRef(_) => None,
        }
    }

    pub(crate) fn linearized_text<B: Binding>(
        &self,
        language: &TargetLanguage,
        effects: &[Effect<'a, B>],
        files: &FileRegistry<'a>,
        memo: &mut HashMap<CodeRange, Option<String>>,
        distributed_indent: Option<usize>,
        logs: &mut AnalysisLogs,
    ) -> Result<Cow<'a, str>> {
        let res: Result<Cow<'a, str>> = match self {
            Self::Empty(_, _, _) => Ok(Cow::Borrowed("")),
            Self::Node(source, node) => {
                let range = CodeRange::from_node(source, node);
                linearize_binding(
                    language,
                    effects,
                    files,
                    memo,
                    source,
                    range,
                    distributed_indent,
                    logs,
                )
                .map(|r| r.0)
            }
            // can't linearize until we update source to point to the entire file
            // otherwise file file pointers won't match
            Self::String(s, r) => Ok(Cow::Owned(
                s[r.start_byte as usize..r.end_byte as usize].into(),
            )),
            Self::FileName(s) => Ok(Cow::Owned(s.to_string_lossy().into())),
            Self::List(source, _parent_node, _field_id) => {
                if let Some(pos) = self.position() {
                    let range = CodeRange::new(pos.start_byte, pos.end_byte, source);
                    linearize_binding(
                        language,
                        effects,
                        files,
                        memo,
                        source,
                        range,
                        distributed_indent,
                        logs,
                    )
                    .map(|r| r.0)
                } else {
                    Ok("".into())
                }
            }
            Self::ConstantRef(c) => Ok(Cow::Owned(c.to_string())),
        };
        res
    }

    pub fn text(&self) -> String {
        match self {
            Self::Empty(_, _, _) => "".to_string(),
            Self::Node(source, node) => {
                NodeWithSource::new(node.clone(), source).text().to_string()
            }
            Self::String(s, r) => s[r.start_byte as usize..r.end_byte as usize].into(),
            Self::FileName(s) => s.to_string_lossy().into(),
            Self::List(source, _, _) => {
                if let Some(pos) = self.position() {
                    source[pos.start_byte as usize..pos.end_byte as usize].to_string()
                } else {
                    "".to_string()
                }
            }
            Self::ConstantRef(c) => c.to_string(),
        }
    }

    pub fn source(&self) -> Option<&'a str> {
        match self {
            Self::Empty(source, _, _) => Some(source),
            Self::Node(source, _) => Some(source),
            Self::String(source, _) => Some(source),
            Self::List(source, _, _) => Some(source),
            Self::FileName(..) | Self::ConstantRef(..) => None,
        }
    }

    /// Returns the constant this binding binds to, if and only if it is a constant binding.
    pub fn as_constant(&self) -> Option<&Constant> {
        if let Self::ConstantRef(constant) = self {
            Some(constant)
        } else {
            None
        }
    }

    /// Returns the path of this binding, if and only if it is a filename binding.
    pub fn as_filename(&self) -> Option<&Path> {
        if let Self::FileName(path) = self {
            Some(path)
        } else {
            None
        }
    }

    /// Returns the node of this binding, if and only if it is a node binding.
    pub(crate) fn as_node(&self) -> Option<NodeWithSource<'a>> {
        if let Self::Node(source, node) = self {
            Some(NodeWithSource::new(node.clone(), source))
        } else {
            None
        }
    }

    /// Returns `true` if and only if this binding is bound to a list.
    pub(crate) fn is_list(&self) -> bool {
        matches!(self, Self::List(..))
    }

    /// Returns an iterator over the items in a list.
    ///
    /// Returns `None` if the binding is not bound to a list.
    pub(crate) fn list_items(&self) -> Option<impl Iterator<Item = NodeWithSource<'a>> + Clone> {
        match self {
            Self::List(src, node, field_id) => {
                let field_id = *field_id;
                let mut cursor = node.walk();
                cursor.goto_first_child();
                let mut done = false;
                Some(
                    iter::from_fn(move || {
                        // seems to me that the external loop is unnecessary, but was
                        // getting an infinite loop without it.
                        #[allow(clippy::never_loop)]
                        while !done {
                            while cursor.field_id() != Some(field_id) {
                                if !cursor.goto_next_sibling() {
                                    return None;
                                }
                            }
                            let result = cursor.node();
                            if !cursor.goto_next_sibling() {
                                done = true;
                            }
                            return Some(result);
                        }
                        None
                    })
                    .filter(|child| child.is_named())
                    .map(|named_child| NodeWithSource::new(named_child, src)),
                )
            }
            Self::Empty(..)
            | Self::Node(..)
            | Self::String(..)
            | Self::ConstantRef(..)
            | Self::FileName(..) => None,
        }
    }

    /// Returns the parent node of this binding.
    ///
    /// Returns `None` if the binding has no relation to a node.
    pub(crate) fn parent_node(&self) -> Option<NodeWithSource<'a>> {
        match self {
            Self::Node(src, node) => node.parent().map(|parent| NodeWithSource::new(parent, src)),
            Self::List(src, node, _) => Some(NodeWithSource::new(node.clone(), src)),
            Self::Empty(src, node, _) => Some(NodeWithSource::new(node.clone(), src)),
            Self::String(..) | Self::FileName(..) | Self::ConstantRef(..) => None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Empty(..) => false,
            Self::List(_, node, field_id) => {
                let child_count = children_by_field_id_count(node, *field_id);
                child_count > 0
            }
            Self::Node(..) => true,
            // This refers to a slice of the source code, not a Grit string literal, so it is truthy
            Self::String(..) => true,
            Self::FileName(_) => true,
            Self::ConstantRef(c) => c.is_truthy(),
        }
    }

    pub(crate) fn log_empty_field_rewrite_error(
        &self,
        language: &TargetLanguage,
        logs: &mut AnalysisLogs,
    ) -> Result<()> {
        match self {
            Self::Empty(src, node, field) | Self::List(src, node, field) => {
                let range: Range = node.range().into();
                let log = AnalysisLogBuilder::default()
                        .level(441_u16)
                        .source(*src)
                        .position(range.start)
                        .range(range)
                        .message(format!(
                            "Error: failed to rewrite binding, cannot derive range of empty field {} of node {}",
                            language.get_ts_language().field_name_for_id(*field).unwrap(),
                            node.kind()
                        ))
                        .build()?;
                logs.push(log);
            }
            Self::String(_, _) | Self::FileName(_) | Self::Node(_, _) | Self::ConstantRef(_) => {}
        }

        Ok(())
    }
}
