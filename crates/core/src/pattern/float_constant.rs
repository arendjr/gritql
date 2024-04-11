use super::{
    patterns::{Matcher, Name},
    resolved_pattern::ResolvedPattern,
    state::State,
};
use crate::{binding::Binding, context::Context};
use anyhow::Result;
use marzano_util::analysis_logs::AnalysisLogs;

#[derive(Debug, Clone)]
pub struct FloatConstant {
    pub value: f64,
}

impl FloatConstant {
    pub fn new(value: f64) -> Self {
        Self { value }
    }
}

impl Name for FloatConstant {
    fn name(&self) -> &'static str {
        "DOUBLE_CONSTANT"
    }
}

impl Matcher for FloatConstant {
    fn execute<'a, B: Binding>(
        &'a self,
        binding: &ResolvedPattern<'a, B>,
        state: &mut State<'a, B>,
        _context: &'a impl Context,
        _logs: &mut AnalysisLogs,
    ) -> Result<bool> {
        let text = binding.text(&state.files)?;
        let parsed_double = text.parse::<f64>()?;
        Ok(parsed_double == self.value)
    }
}
