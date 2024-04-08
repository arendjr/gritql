use super::{
    back_tick_compiler::BackTickCompiler, compiler::NodeCompilationContext,
    node_compiler::NodeCompiler, variable_compiler::VariableCompiler,
};
use crate::pattern::regex::{RegexLike, RegexPattern};
use anyhow::{anyhow, bail, Result};
use grit_util::AstNode;
use marzano_language::language::Language;
use marzano_util::{
    analysis_logs::AnalysisLogBuilder, node_with_source::NodeWithSource, position::Range,
};

pub(crate) struct RegexCompiler;

impl NodeCompiler for RegexCompiler {
    type TargetPattern = RegexPattern;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        if context.is_rhs {
            bail!("regex patterns are not allowed on the right-hand side of a rule")
        }
        let regex_node = node
            .child_by_field_name("regex")
            .ok_or_else(|| anyhow!("malformed regex, check the parser"))?;

        let regex = if regex_node.node.kind() == "regex" {
            let regex = regex_node.text().trim().to_string();
            let regex = regex
                .strip_prefix("r\"")
                .ok_or_else(|| anyhow!("invalid regex prefix"))?
                .strip_suffix('\"')
                .ok_or_else(|| anyhow!("invalid regex postfix"))?;

            RegexLike::Regex(regex.to_string())
        } else {
            let back_tick_node = regex_node
                .child_by_field_name("snippet")
                .ok_or_else(|| anyhow!("malformed regex, check the parser"))?;
            let regex = regex_node.text().trim().to_string();
            if !context.lang.metavariable_regex().is_match(&regex) {
                let range: Range = regex_node.range().into();
                let alternative = format!(
                    "r\"{}\"",
                    regex
                        .strip_prefix("r`")
                        .ok_or_else(|| anyhow!("invalid regex prefix"))?
                        .strip_suffix('`')
                        .ok_or_else(|| anyhow!("invalid regex postfix"))?
                );
                let log = AnalysisLogBuilder::default()
                .level(441_u16)
                .file(context.file)
                .source(node.source)
                .position(range.start)
                .range(range)
                .message(
                    format!("Warning: unnecessary use of metavariable snippet syntax without metavariables. Replace {regex} with {alternative}"))
                .build()?;
                context.logs.push(log);
            }
            let pattern = BackTickCompiler::from_node(back_tick_node, context)?;
            RegexLike::Pattern(Box::new(pattern))
        };

        let variables = node
            .named_children_by_field_name("variables")
            .map(|n| VariableCompiler::from_node(n, context).unwrap());

        let variables: Vec<_> = variables.collect();

        Ok(RegexPattern::new(regex, variables))
    }
}
