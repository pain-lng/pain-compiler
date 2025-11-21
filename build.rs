// Build script for LLVM linking (only when JIT feature is enabled) and icon embedding

fn main() {
    // Embed Windows icon if available
    #[cfg(target_os = "windows")]
    {
        let is_primary = std::env::var("CARGO_PRIMARY_PACKAGE").ok().as_deref() == Some("1");
        if !is_primary {
            // Built as a dependency (e.g. for pain-lsp): skip embedding to avoid
            // duplicate VERSION resources during downstream linking.
        } else {
            // Only embed icon when building this crate directly.
            let icon_path = std::path::Path::new("resources/icons/windows/pain.ico");
            if icon_path.exists() {
                let mut res = winres::WindowsResource::new();
                res.set_icon(icon_path.to_str().unwrap());
                // Try to use GNU windres instead of CVTRES if available
                // This avoids CVTRES errors
                if std::env::var("CARGO_CFG_TARGET_ENV").unwrap_or_default() == "gnu" {
                    // GNU toolchain - should work fine
                }
                if let Err(e) = res.compile() {
                    eprintln!("cargo:warning=Failed to embed Windows icon: {}", e);
                    eprintln!(
                        "cargo:warning=This is a known issue with CVTRES on some Windows setups"
                    );
                    eprintln!("cargo:warning=Build will continue without icon");
                    eprintln!(
                        "cargo:warning=Icon file is available at: {}",
                        icon_path.display()
                    );
                }
            } else {
                println!(
                    "cargo:warning=Windows icon not found ({}), skipping embed",
                    icon_path.display()
                );
            }
        }
    }

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
