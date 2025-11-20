// Build script for LLVM linking (only when JIT feature is enabled)

fn main() {
    // Only configure LLVM linking if JIT feature is enabled
    #[cfg(feature = "jit")]
    {
        #[cfg(target_os = "windows")]
        {
            // Default LLVM path on Windows
            let llvm_path = std::env::var("LLVM_SYS_211_PREFIX")
                .unwrap_or_else(|_| r"C:\Program Files\LLVM".to_string());

            // Tell cargo to link with LLVM libraries
            // Note: Using the path as-is (spaces in path should be handled by cargo)
            println!("cargo:rustc-link-search=native={}\\lib", llvm_path);

            // Link with required LLVM libraries for ORC JIT
            println!("cargo:rustc-link-lib=LLVMOrcJIT");
            println!("cargo:rustc-link-lib=LLVMExecutionEngine");
            println!("cargo:rustc-link-lib=LLVMRuntimeDyld");
            println!("cargo:rustc-link-lib=LLVMIRReader");
            println!("cargo:rustc-link-lib=LLVMAsmParser");
            println!("cargo:rustc-link-lib=LLVMCore");
            println!("cargo:rustc-link-lib=LLVMSupport");
        }

        #[cfg(not(target_os = "windows"))]
        {
            // On Unix-like systems, llvm-sys will find LLVM via pkg-config or standard paths
            // Just ensure we link with required libraries
            println!("cargo:rustc-link-lib=LLVMOrcJIT");
            println!("cargo:rustc-link-lib=LLVMExecutionEngine");
            println!("cargo:rustc-link-lib=LLVMRuntimeDyld");
            println!("cargo:rustc-link-lib=LLVMIRReader");
            println!("cargo:rustc-link-lib=LLVMAsmParser");
            println!("cargo:rustc-link-lib=LLVMCore");
            println!("cargo:rustc-link-lib=LLVMSupport");
        }
    }
}
