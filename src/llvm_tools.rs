// LLVM tools integration for AOT compilation

use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;

/// Check if llc is available
pub fn check_llc_available() -> bool {
    Command::new("llc").arg("--version").output().is_ok()
}

/// Check if clang is available
pub fn check_clang_available() -> bool {
    Command::new("clang").arg("--version").output().is_ok()
}

/// Find available LLVM compiler (llc or clang)
fn find_llvm_compiler() -> io::Result<String> {
    // Try llc first
    if check_llc_available() {
        return Ok("llc".to_string());
    }

    // Fallback to clang
    if check_clang_available() {
        return Ok("clang".to_string());
    }

    // Try clang from LLVM installation on Windows
    #[cfg(target_os = "windows")]
    {
        let llvm_clang = r"C:\Program Files\LLVM\bin\clang.exe";
        if Path::new(llvm_clang).exists() {
            return Ok(llvm_clang.to_string());
        }
    }

    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "Neither llc nor clang found in PATH. Please install LLVM and add it to PATH.\n\
         On Windows: Install LLVM from https://llvm.org/builds/ or via chocolatey: choco install llvm\n\
         On Linux: sudo apt-get install llvm clang (or equivalent)\n\
         On macOS: brew install llvm"
    ))
}

/// Compile LLVM IR to object file using llc or clang
pub fn compile_llvm_ir_to_object(
    llvm_ir_path: &Path,
    object_path: &Path,
    target_triple: Option<&str>,
) -> io::Result<()> {
    let compiler = find_llvm_compiler()?;

    let mut cmd = Command::new(&compiler);

    if compiler.ends_with("llc") || compiler == "llc" {
        // Use llc
        cmd.arg("-filetype=obj");
        cmd.arg("-o").arg(object_path);

        if let Some(triple) = target_triple {
            cmd.arg("-mtriple").arg(triple);
        }

        cmd.arg(llvm_ir_path);
    } else {
        // Use clang to compile LLVM IR to object file
        cmd.arg("-c");
        cmd.arg("-o").arg(object_path);

        if let Some(triple) = target_triple {
            cmd.arg("-target").arg(triple);
        }

        cmd.arg(llvm_ir_path);
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(io::Error::other(format!(
            "{} failed:\nSTDERR: {}\nSTDOUT: {}",
            compiler, stderr, stdout
        )));
    }

    Ok(())
}

/// Link object file to executable
pub fn link_object_to_executable(
    object_path: &Path,
    executable_path: &Path,
    target_triple: Option<&str>,
) -> io::Result<()> {
    link_objects_to_executable(&[object_path], executable_path, target_triple)
}

/// Link multiple object files to executable
fn link_objects_to_executable(
    object_paths: &[&Path],
    executable_path: &Path,
    target_triple: Option<&str>,
) -> io::Result<()> {
    // Detect linker based on target triple or platform
    let linker = detect_linker(target_triple)?;

    let mut cmd = Command::new(&linker);

    match linker.as_str() {
        "clang" | "gcc" => {
            // Unix-like systems: use clang/gcc as linker
            cmd.arg("-o").arg(executable_path);
            for obj_path in object_paths {
                if obj_path.exists() {
                    cmd.arg(obj_path);
                }
            }
        }
        "link" => {
            // Windows MSVC linker
            cmd.arg("/OUT:").arg(executable_path);
            for obj_path in object_paths {
                if obj_path.exists() {
                    cmd.arg(obj_path);
                }
            }
        }
        "ld" => {
            // Direct linker (less common)
            cmd.arg("-o").arg(executable_path);
            for obj_path in object_paths {
                if obj_path.exists() {
                    cmd.arg(obj_path);
                }
            }
        }
        _ => {
            return Err(io::Error::other(format!("Unknown linker: {}", linker)));
        }
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(format!("Linker failed: {}", stderr)));
    }

    Ok(())
}

/// Detect appropriate linker for the target platform
fn detect_linker(target_triple: Option<&str>) -> io::Result<String> {
    // Check target triple first
    if let Some(triple) = target_triple {
        if triple.contains("windows") || triple.contains("msvc") {
            // Try to find link.exe (MSVC) or use clang
            if Command::new("link").output().is_ok() {
                return Ok("link".to_string());
            }
            if Command::new("clang").output().is_ok() {
                return Ok("clang".to_string());
            }
        } else if triple.contains("linux") || triple.contains("darwin") || triple.contains("unix") {
            // Unix-like: prefer clang, fallback to gcc
            if Command::new("clang").output().is_ok() {
                return Ok("clang".to_string());
            }
            if Command::new("gcc").output().is_ok() {
                return Ok("gcc".to_string());
            }
        }
    }

    // Fallback: detect from current platform
    #[cfg(target_os = "windows")]
    {
        if Command::new("link").output().is_ok() {
            return Ok("link".to_string());
        }
        if Command::new("clang").output().is_ok() {
            return Ok("clang".to_string());
        }
    }

    #[cfg(not(target_os = "windows"))]
    {
        if Command::new("clang").output().is_ok() {
            return Ok("clang".to_string());
        }
        if Command::new("gcc").output().is_ok() {
            return Ok("gcc".to_string());
        }
    }

    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "No suitable linker found. Please install clang, gcc, or MSVC linker.",
    ))
}

/// Compile C runtime library to object file
fn compile_stdlib_runtime(object_path: &Path, target_triple: Option<&str>) -> io::Result<()> {
    // Find stdlib_runtime.c in the source directory
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .map_err(|_| io::Error::new(io::ErrorKind::NotFound, "CARGO_MANIFEST_DIR not set"))?;
    let runtime_c_path = Path::new(&manifest_dir)
        .join("src")
        .join("stdlib_runtime.c");

    if !runtime_c_path.exists() {
        // If runtime file doesn't exist, skip compilation
        return Ok(());
    }

    let compiler = find_llvm_compiler()?;
    let mut cmd = Command::new(&compiler);

    cmd.arg("-c");
    cmd.arg("-o").arg(object_path);

    if let Some(triple) = target_triple {
        cmd.arg("-target").arg(triple);
    }

    cmd.arg(&runtime_c_path);

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(format!(
            "Failed to compile stdlib runtime: {}",
            stderr
        )));
    }

    Ok(())
}

/// PGO mode for compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PgoMode {
    /// Normal compilation (no PGO)
    None,
    /// Generate profile data
    Generate,
    /// Use profile data for optimization
    Use,
}

/// Compile LLVM IR to executable in one step
pub fn compile_to_executable(
    llvm_ir_path: &Path,
    executable_path: &Path,
    target_triple: Option<&str>,
    keep_intermediates: bool,
) -> io::Result<()> {
    compile_to_executable_with_pgo(
        llvm_ir_path,
        executable_path,
        target_triple,
        keep_intermediates,
        PgoMode::None,
        None,
    )
}

/// Compile LLVM IR to executable with PGO support
pub fn compile_to_executable_with_pgo(
    llvm_ir_path: &Path,
    executable_path: &Path,
    target_triple: Option<&str>,
    keep_intermediates: bool,
    pgo_mode: PgoMode,
    profile_path: Option<&Path>,
) -> io::Result<()> {
    // Try to use clang directly (faster, one step)
    let compiler = find_llvm_compiler()?;

    if compiler.contains("clang") || compiler == "clang" {
        // Use clang to compile LLVM IR directly to executable
        let pgo_msg = match pgo_mode {
            PgoMode::Generate => " (PGO: generating profile)",
            PgoMode::Use => " (PGO: using profile)",
            PgoMode::None => "",
        };
        println!("Compiling LLVM IR to executable using clang{}...", pgo_msg);
        let mut cmd = Command::new(&compiler);
        cmd.arg("-o").arg(executable_path);

        if let Some(triple) = target_triple {
            cmd.arg("-target").arg(triple);
        }

        // Add PGO flags
        match pgo_mode {
            PgoMode::Generate => {
                // Generate profile data
                cmd.arg("-fprofile-generate");
                // Set profile directory (default to current directory)
                if let Some(prof_dir) = profile_path {
                    cmd.arg("-fprofile-dir").arg(prof_dir);
                }
            }
            PgoMode::Use => {
                // Use profile data for optimization
                if let Some(prof_path) = profile_path {
                    cmd.arg("-fprofile-use").arg(prof_path);
                } else {
                    // Try to find default.profdata in current directory
                    let default_profdata = executable_path
                        .parent()
                        .unwrap_or(Path::new("."))
                        .join("default.profdata");
                    if default_profdata.exists() {
                        cmd.arg("-fprofile-use").arg(&default_profdata);
                    } else {
                        return Err(io::Error::other(
                            "PGO use mode requires profile data. Use --pgo-profile to specify path or ensure default.profdata exists."
                        ));
                    }
                }
            }
            PgoMode::None => {}
        }

        // Compile and link stdlib runtime if it exists
        // Note: In release builds, CARGO_MANIFEST_DIR might not be available
        // So we try to find the file relative to the executable or skip it
        if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
            let runtime_c_path = Path::new(&manifest_dir)
                .join("src")
                .join("stdlib_runtime.c");
            if runtime_c_path.exists() {
                cmd.arg(&runtime_c_path);
            }
        }

        cmd.arg(llvm_ir_path);

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            return Err(io::Error::other(format!(
                "clang failed:\nSTDERR: {}\nSTDOUT: {}",
                stderr, stdout
            )));
        }

        println!("✓ Executable generated: {:?}", executable_path);

        // Clean up intermediate files if not keeping them
        if !keep_intermediates {
            let _ = fs::remove_file(llvm_ir_path);
        }

        return Ok(());
    }

    // Fallback: use llc + linker (two steps)
    // Create temporary object file
    let object_path = executable_path.with_extension("o");
    let runtime_object_path = executable_path.with_extension("runtime.o");

    // Step 1: Compile LLVM IR to object file
    println!("Compiling LLVM IR to object file...");
    compile_llvm_ir_to_object(llvm_ir_path, &object_path, target_triple)?;
    println!("✓ Object file generated: {:?}", object_path);

    // Step 1.5: Compile stdlib runtime if it exists
    if compile_stdlib_runtime(&runtime_object_path, target_triple).is_ok() {
        println!("✓ Stdlib runtime compiled: {:?}", runtime_object_path);
    }

    // Step 2: Link object files to executable
    println!("Linking object files to executable...");
    link_objects_to_executable(
        &[&object_path, &runtime_object_path],
        executable_path,
        target_triple,
    )?;
    println!("✓ Executable generated: {:?}", executable_path);

    // Clean up intermediate files if not keeping them
    if !keep_intermediates {
        let _ = fs::remove_file(&object_path);
        let _ = fs::remove_file(&runtime_object_path);
        let _ = fs::remove_file(llvm_ir_path);
    }

    Ok(())
}

/// Merge profile data files into a single profile
/// This is typically needed after collecting profile data with -fprofile-generate
pub fn merge_profiles(profile_files: &[&Path], output_path: &Path) -> io::Result<()> {
    // Try to find llvm-profdata
    let profdata = find_llvm_profdata()?;

    let mut cmd = Command::new(&profdata);
    cmd.arg("merge");
    cmd.arg("-o").arg(output_path);

    for profile_file in profile_files {
        if profile_file.exists() {
            cmd.arg(profile_file);
        }
    }

    let output = cmd.output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::other(format!(
            "llvm-profdata merge failed: {}",
            stderr
        )));
    }

    Ok(())
}

/// Find llvm-profdata tool
fn find_llvm_profdata() -> io::Result<String> {
    // Try standard name first
    if Command::new("llvm-profdata")
        .arg("--version")
        .output()
        .is_ok()
    {
        return Ok("llvm-profdata".to_string());
    }

    // Try with version suffix
    for version in &["21", "20", "19", "18"] {
        let name = format!("llvm-profdata-{}", version);
        if Command::new(&name).arg("--version").output().is_ok() {
            return Ok(name);
        }
    }

    // Try Windows path
    #[cfg(target_os = "windows")]
    {
        let llvm_profdata = r"C:\Program Files\LLVM\bin\llvm-profdata.exe";
        if Path::new(llvm_profdata).exists() {
            return Ok(llvm_profdata.to_string());
        }
    }

    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "llvm-profdata not found. Please install LLVM and add it to PATH.",
    ))
}

/// Detect target triple from current platform
pub fn detect_target_triple() -> String {
    if cfg!(all(target_os = "windows", target_arch = "x86_64")) {
        "x86_64-pc-windows-msvc".to_string()
    } else if cfg!(all(target_os = "windows", target_arch = "x86")) {
        "i686-pc-windows-msvc".to_string()
    } else if cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        "x86_64-unknown-linux-gnu".to_string()
    } else if cfg!(all(target_os = "linux", target_arch = "aarch64")) {
        "aarch64-unknown-linux-gnu".to_string()
    } else if cfg!(all(target_os = "macos", target_arch = "x86_64")) {
        "x86_64-apple-darwin".to_string()
    } else if cfg!(all(target_os = "macos", target_arch = "aarch64")) {
        "aarch64-apple-darwin".to_string()
    } else {
        // Default fallback for unknown platforms
        "x86_64-unknown-linux-gnu".to_string()
    }
}
