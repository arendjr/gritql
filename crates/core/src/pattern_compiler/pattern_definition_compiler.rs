use super::{
    and_compiler::AndCompiler, compiler::NodeCompilationContext, node_compiler::NodeCompiler,
};
use crate::pattern::{pattern_definition::PatternDefinition, variable::get_variables};
use anyhow::{anyhow, Result};
use grit_util::AstNode;
use marzano_util::node_with_source::NodeWithSource;
use std::collections::BTreeMap;

pub(crate) struct PatternDefinitionCompiler;

impl NodeCompiler for PatternDefinitionCompiler {
    type TargetPattern = PatternDefinition;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        // TODO: make sure pattern definitions are only allowed at the top level
        let name = node
            .child_by_field_name("name")
            .ok_or_else(|| anyhow!("missing name of patternDefinition"))?;
        let name = name.text().trim();
        let scope_index = context.vars_array.len();
        context.vars_array.push(vec![]);
        let mut context = context.with_local_scope(scope_index, &mut BTreeMap::new());
        // important that this occurs first, as calls assume
        // that parameters are registered first
        let params = get_variables(
            &context
                .pattern_definition_info
                .get(name)
                .ok_or_else(|| anyhow!("cannot get info for pattern {name}"))?
                .parameters,
            &mut context,
        )?;

        let body = node
            .child_by_field_name("body")
            .ok_or_else(|| anyhow!("missing body of patternDefinition"))?;
        let body = AndCompiler::from_node(body, &mut context)?;
        let pattern_def = PatternDefinition::new(
            name.to_owned(),
            scope_index,
            params,
            context.vars.values().cloned().collect(),
            body,
        );
        Ok(pattern_def)
    }
}
