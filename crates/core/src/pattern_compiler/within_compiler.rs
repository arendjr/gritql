use super::{
    compiler::NodeCompilationContext, node_compiler::NodeCompiler,
    pattern_compiler::PatternCompiler,
};
use crate::pattern::within::Within;
use anyhow::{anyhow, Result};
use marzano_util::node_with_source::NodeWithSource;

pub(crate) struct WithinCompiler;

impl NodeCompiler for WithinCompiler {
    type TargetPattern = Within;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        let within = node
            .child_by_field_name("pattern")
            .ok_or_else(|| anyhow!("missing pattern of pattern within"))?;
        let within = PatternCompiler::from_node(within, &mut context.with_rhs(false))?;
        Ok(Within::new(within))
    }
}
