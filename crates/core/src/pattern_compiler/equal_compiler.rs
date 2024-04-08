use super::{
    compiler::NodeCompilationContext, node_compiler::NodeCompiler,
    pattern_compiler::PatternCompiler,
};
use crate::pattern::{equal::Equal, patterns::Pattern};
use anyhow::{anyhow, Result};
use marzano_util::node_with_source::NodeWithSource;

pub(crate) struct EqualCompiler;

impl NodeCompiler for EqualCompiler {
    type TargetPattern = Equal;

    fn from_node(
        node: NodeWithSource,
        context: &mut NodeCompilationContext,
    ) -> Result<Self::TargetPattern> {
        let mut context = context.with_rhs(true);
        let variable = node
            .child_by_field_name("left")
            .ok_or_else(|| anyhow!("missing lhs of predicateEqual"))?;
        let variable = PatternCompiler::from_node(variable, &mut context)?;
        let pattern = node
            .child_by_field_name("right")
            .ok_or_else(|| anyhow!("missing rhs of predicateEqual"))?;
        let pattern = PatternCompiler::from_node(pattern, &mut context)?;
        if let Pattern::Variable(var) = variable {
            Ok(Equal::new(var, pattern))
        } else {
            Err(anyhow!(
                "predicateEqual must have a variable as first argument",
            ))
        }
    }
}
