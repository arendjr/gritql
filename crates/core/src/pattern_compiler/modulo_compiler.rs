use super::{
    compiler::NodeCompilationContext, node_compiler::NodeCompiler,
    pattern_compiler::PatternCompiler,
};
use crate::pattern::modulo::Modulo;
use anyhow::{anyhow, Result};
use marzano_util::node_with_source::NodeWithSource;

pub(crate) struct ModuloCompiler;

impl NodeCompiler for ModuloCompiler {
    type TargetPattern = Modulo;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        let mut context = context.with_rhs(false);

        let left = node
            .child_by_field_name("left")
            .ok_or_else(|| anyhow!("missing left of modulo"))?;
        let left = PatternCompiler::from_node(left, &mut context)?;

        let right = node
            .child_by_field_name("right")
            .ok_or_else(|| anyhow!("missing right of modulo"))?;
        let right = PatternCompiler::from_node(right, &mut context)?;

        Ok(Modulo::new(left, right))
    }
}
