use super::{
    compiler::NodeCompilationContext, node_compiler::NodeCompiler,
    pattern_compiler::PatternCompiler,
};
use crate::pattern::subtract::Subtract;
use anyhow::{anyhow, Result};
use marzano_util::node_with_source::NodeWithSource;

pub(crate) struct SubtractCompiler;

impl NodeCompiler for SubtractCompiler {
    type TargetPattern = Subtract;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        let mut context = context.with_rhs(false);
        let left = node
            .child_by_field_name("left")
            .ok_or_else(|| anyhow!("missing left of subtract"))?;
        let left = PatternCompiler::from_node(left, &mut context)?;

        let right = node
            .child_by_field_name("right")
            .ok_or_else(|| anyhow!("missing right of subtract"))?;
        let right = PatternCompiler::from_node(right, &mut context)?;

        Ok(Subtract::new(left, right))
    }
}
