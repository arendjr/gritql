use super::compiler::NodeCompilationContext;
use anyhow::Result;
use marzano_util::node_with_source::NodeWithSource;

pub(crate) trait NodeCompiler {
    type TargetPattern;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern>;
}
