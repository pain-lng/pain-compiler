// LLVM JIT compilation module
// Provides runtime code generation using LLVM ExecutionEngine
// Uses a simpler approach: compile LLVM IR to object file and load it dynamically

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

#[cfg(feature = "jit")]
use crate::llvm_tools::find_llvm_compiler;

/// JIT engine for compiling LLVM IR to machine code at runtime
pub struct OrcJitEngine {
    // Temporary directory for compiled modules
    temp_dir: PathBuf,
    // Counter for unique module names
    module_counter: AtomicU64,
}

impl OrcJitEngine {
    /// Create a new JIT engine
    pub fn new() -> Result<Self, String> {
        let temp_dir = std::env::temp_dir().join("pain_jit");
        fs::create_dir_all(&temp_dir)
            .map_err(|e| format!("Failed to create temp directory: {}", e))?;

        Ok(Self {
            temp_dir,
            module_counter: AtomicU64::new(0),
        })
    }

    /// Compile LLVM IR module to machine code and get function pointer
    #[cfg(feature = "jit")]
    pub fn compile_module(&self, llvm_ir: &str, function_name: &str) -> Result<*const u8, String> {
        // Generate unique module name
        let module_id = self.module_counter.fetch_add(1, Ordering::SeqCst);
        let module_name = format!("jit_module_{}", module_id);
        
        // Create temporary files
        let ir_file = self.temp_dir.join(format!("{}.ll", module_name));
        let obj_file = self.temp_dir.join(format!("{}.o", module_name));

        // Write LLVM IR to file
        fs::write(&ir_file, llvm_ir)
            .map_err(|e| format!("Failed to write IR file: {}", e))?;

        // Compile IR to object file using llc
        let llc = find_llvm_compiler().map_err(|e| format!("Failed to find LLVM compiler: {}", e))?;
        let llc_path = if llc.contains("clang") {
            // If we have clang, try to find llc in the same directory
            let clang_dir = std::path::Path::new(&llc).parent()
                .ok_or("Invalid clang path")?;
            clang_dir.join("llc")
        } else {
            PathBuf::from("llc")
        };

        // Try to compile with llc (if available)
        let compile_result = if llc_path.exists() {
            Command::new(&llc_path)
                .arg("-filetype=obj")
                .arg("-o")
                .arg(&obj_file)
                .arg(&ir_file)
                .output()
        } else {
            // Fallback: try system llc
            Command::new("llc")
                .arg("-filetype=obj")
                .arg("-o")
                .arg(&obj_file)
                .arg(&ir_file)
                .output()
        };

        // Determine shared library extension
        #[cfg(target_os = "windows")]
        let so_ext = "dll";
        #[cfg(target_os = "macos")]
        let so_ext = "dylib";
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        let so_ext = "so";
        
        let so_file = self.temp_dir.join(format!("{}.{}", module_name, so_ext));

        if compile_result.is_err() || !compile_result.as_ref().unwrap().status.success() {
            // Fallback: use clang to compile IR directly to shared library
            let clang_result = Command::new(&llc)
                .arg("-shared")
                .arg("-fPIC")
                .arg("-o")
                .arg(&so_file)
                .arg(&ir_file)
                .output();

            if clang_result.is_err() || !clang_result.as_ref().unwrap().status.success() {
                let error = if let Ok(output) = &clang_result {
                    String::from_utf8_lossy(&output.stderr).to_string()
                } else {
                    "Unknown error".to_string()
                };
                return Err(format!("Failed to compile LLVM IR: {}", error));
            }
        } else {
            // Link object file to shared library
            #[cfg(target_os = "windows")]
            let linker = "link.exe";
            #[cfg(target_os = "macos")]
            let linker = "clang";
            #[cfg(not(any(target_os = "windows", target_os = "macos")))]
            let linker = "ld";

            let mut link_cmd = Command::new(linker);
            #[cfg(target_os = "windows")]
            {
                link_cmd.arg("/DLL").arg("/OUT:").arg(&so_file).arg(&obj_file);
            }
            #[cfg(target_os = "macos")]
            {
                link_cmd.arg("-shared").arg("-o").arg(&so_file).arg(&obj_file);
            }
            #[cfg(not(any(target_os = "windows", target_os = "macos")))]
            {
                link_cmd.arg("-shared").arg("-o").arg(&so_file).arg(&obj_file);
            }

            let link_result = link_cmd.output();

            if link_result.is_err() || !link_result.as_ref().unwrap().status.success() {
                return Err("Failed to link object file".to_string());
            }
        }

        // Load shared library and get function pointer
        #[cfg(feature = "jit")]
        {
            use libloading::Library;
            
            unsafe {
                let lib = Library::new(&so_file)
                    .map_err(|e| format!("Failed to load shared library: {}", e))?;
                
                // Get function pointer (assuming it returns i64 for now)
                let func: libloading::Symbol<unsafe extern "C" fn() -> i64> = lib
                    .get(function_name.as_bytes())
                    .map_err(|e| format!("Failed to find function '{}': {}", function_name, e))?;

                // Get the raw pointer
                let func_ptr = *func as *const u8;
                
                // Leak the library to keep it loaded
                std::mem::forget(lib);

                Ok(func_ptr)
            }
        }
    }

    /// Compile LLVM IR module (fallback when JIT feature is disabled)
    #[cfg(not(feature = "jit"))]
    pub fn compile_module(&self, _llvm_ir: &str, _function_name: &str) -> Result<*const u8, String> {
        Err("JIT feature is not enabled. Build with --features jit".to_string())
    }
}

impl Drop for OrcJitEngine {
    fn drop(&mut self) {
        // Clean up temporary files
        let _ = fs::remove_dir_all(&self.temp_dir);
    }
}

// Fallback implementation when JIT feature is disabled
#[cfg(not(feature = "jit"))]
pub struct OrcJitEngine;

#[cfg(not(feature = "jit"))]
impl OrcJitEngine {
    pub fn new() -> Result<Self, String> {
        Err("JIT feature is not enabled. Build with --features jit".to_string())
    }

    pub fn compile_module(&self, _llvm_ir: &str, _function_name: &str) -> Result<*const u8, String> {
        Err("JIT feature is not enabled".to_string())
    }
}

