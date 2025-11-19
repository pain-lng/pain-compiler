// JIT compilation engine for Pain
// Compiles Pain IR to machine code at runtime

use crate::codegen::CodeGenerator;
use crate::ir::IrProgram;
use crate::optimizations::Optimizer;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// JIT-compiled function handle
#[derive(Debug, Clone)]
pub struct JitFunction {
    /// Function name
    pub name: String,
    /// Compiled machine code address (when using real JIT)
    pub code_ptr: Option<*const u8>,
    /// LLVM IR for this function (cached)
    pub llvm_ir: String,
}

/// JIT compilation engine
pub struct JitEngine {
    /// Cache of compiled functions
    function_cache: Rc<RefCell<HashMap<String, JitFunction>>>,
    /// Optimized IR program
    ir: IrProgram,
}

impl JitEngine {
    /// Create a new JIT engine from Pain IR
    pub fn new(ir: IrProgram) -> Self {
        // Optimize IR
        let optimized_ir = Optimizer::optimize(ir);

        Self {
            function_cache: Rc::new(RefCell::new(HashMap::new())),
            ir: optimized_ir,
        }
    }

    /// Compile a function to machine code (returns LLVM IR for now)
    pub fn compile_function(&self, function_name: &str) -> Result<JitFunction, String> {
        // Check cache first
        {
            let cache = self.function_cache.borrow();
            if let Some(cached) = cache.get(function_name) {
                return Ok(cached.clone());
            }
        }

        // Find function in IR
        let _func = self
            .ir
            .functions
            .iter()
            .find(|f| f.name == function_name)
            .ok_or_else(|| format!("Function '{}' not found", function_name))?;

        // Generate LLVM IR for this function
        let codegen = CodeGenerator::new(self.ir.clone());
        let full_llvm_ir = codegen.generate();

        // Extract function-specific IR (simplified - in real JIT we'd compile just the function)
        let function_ir = extract_function_ir(&full_llvm_ir, function_name)?;

        let jit_func = JitFunction {
            name: function_name.to_string(),
            code_ptr: None, // TODO: Compile to machine code using LLVM ORC
            llvm_ir: function_ir,
        };

        // Cache the function
        {
            let mut cache = self.function_cache.borrow_mut();
            cache.insert(function_name.to_string(), jit_func.clone());
        }

        Ok(jit_func)
    }

    /// Get cached function or compile if not cached
    pub fn get_or_compile(&self, function_name: &str) -> Result<JitFunction, String> {
        // Check cache
        {
            let cache = self.function_cache.borrow();
            if let Some(cached) = cache.get(function_name) {
                return Ok(cached.clone());
            }
        }

        // Compile and cache
        self.compile_function(function_name)
    }

    /// Clear the function cache
    pub fn clear_cache(&self) {
        let mut cache = self.function_cache.borrow_mut();
        cache.clear();
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> (usize, Vec<String>) {
        let cache = self.function_cache.borrow();
        let names: Vec<String> = cache.keys().cloned().collect();
        (cache.len(), names)
    }
}

/// Extract function-specific LLVM IR from full module IR
fn extract_function_ir(full_ir: &str, function_name: &str) -> Result<String, String> {
    // Simple extraction: find function definition
    // LLVM IR format: "define <return_type> @<function_name>(<params>) {"
    let func_pattern = format!("@{}(", function_name);
    let start_idx = full_ir
        .find(&func_pattern)
        .ok_or_else(|| format!("Function '{}' not found in LLVM IR", function_name))?;

    // Find the start of "define" keyword before the function name
    let define_start = full_ir[..start_idx]
        .rfind("define")
        .ok_or_else(|| format!("Function '{}' not found in LLVM IR", function_name))?;

    // Find the end of the function (next '}' at the same level)
    let mut depth = 0;
    let mut in_function = false;
    let mut end_idx = define_start;

    // Find the opening brace after function signature
    let brace_start = full_ir[start_idx..]
        .find('{')
        .ok_or_else(|| format!("Function '{}' has no body", function_name))?;
    let actual_start = start_idx + brace_start;

    for (i, ch) in full_ir[actual_start..].char_indices() {
        match ch {
            '{' => {
                depth += 1;
                in_function = true;
            }
            '}' => {
                depth -= 1;
                if in_function && depth == 0 {
                    end_idx = start_idx + i + 1;
                    break;
                }
            }
            _ => {}
        }
    }

    if !in_function || depth != 0 {
        return Err(format!(
            "Failed to extract function '{}' from LLVM IR",
            function_name
        ));
    }

    Ok(full_ir[start_idx..end_idx].to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_builder::IrBuilder;
    use crate::parse;

    #[test]
    fn test_jit_engine_creation() {
        let source = "fn add(a: int, b: int) -> int:
    return a + b

fn main() -> int:
    return add(1, 2)";

        let program = parse(source).unwrap();
        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);

        let jit = JitEngine::new(ir);
        let (count, _) = jit.cache_stats();
        assert_eq!(count, 0);
    }

    #[test]
    fn test_jit_compile_function() {
        let source = "fn add(a: int, b: int) -> int:
    return a + b

fn main() -> int:
    return add(1, 2)";

        let program = parse(source).unwrap();
        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);

        let jit = JitEngine::new(ir);
        let func = jit.compile_function("add").unwrap();
        assert_eq!(func.name, "add");
        assert!(!func.llvm_ir.is_empty());

        // Check cache
        let (count, names) = jit.cache_stats();
        assert_eq!(count, 1);
        assert!(names.contains(&"add".to_string()));
    }
}
