use super::{
    compiler::NodeCompilationContext, node_compiler::NodeCompiler,
    pattern_compiler::PatternCompiler,
};
use crate::pattern::{
    bubble::Bubble,
    pattern_definition::PatternDefinition,
    patterns::Pattern,
    variable::{get_variables, register_variable},
};
use anyhow::{anyhow, bail, Result};
use grit_util::AstNode;
use itertools::Itertools;
use marzano_util::node_with_source::NodeWithSource;
use std::collections::BTreeMap;

pub(crate) struct BubbleCompiler;

impl NodeCompiler for BubbleCompiler {
    type TargetPattern = Bubble;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        let local_scope_index = context.vars_array.len();
        context.vars_array.push(vec![]);
        let mut local_context = context.with_local_scope(local_scope_index, &mut BTreeMap::new());
        // important that this occurs first, as calls assume
        // that parameters are registered first

        let parameters: Vec<_> = node
            .named_children_by_field_name("variables")
            .map(|n| (n.text().trim().to_string(), n.range().into()))
            .collect();
        if parameters.iter().unique_by(|n| n.0.clone()).count() != parameters.len() {
            bail!("bubble parameters must be unique, but had a repeated name in its parameters.")
        }
        let params = get_variables(&parameters, &mut local_context)?;

        let body = node
            .child_by_field_name("pattern")
            .ok_or_else(|| anyhow!("missing body of patternDefinition"))?;
        let body = PatternCompiler::from_node(body, &mut local_context.with_rhs(false))?;

        let args = parameters
            .iter()
            .map(|(name, range)| {
                let v = Pattern::Variable(register_variable(name, *range, context)?);
                Ok(v)
            })
            .collect::<Result<Vec<Pattern>>>()?;

        let pattern_def = PatternDefinition::new(
            "<bubble>".to_string(),
            local_scope_index,
            params,
            local_context.vars.values().cloned().collect(),
            body,
        );

        Ok(Bubble::new(pattern_def, args))
    }
}
