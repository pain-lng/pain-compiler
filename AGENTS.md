# pain-compiler — guide for AI agents

The compiler crate of the Pain language: library `pain_compiler` and the `pain` CLI.
It is a member of the workspace in the parent repository (`pain-lng/pain`) and depends
on `../pain-runtime` by path, so build and test it from inside that workspace.
General rules for the project are in the parent repository's `AGENTS.md`.

## Pipeline and where it lives

```
source ─ lexer.rs ─ parser/mod.rs ─ ast.rs ─ type_checker.rs ─┬─ interpreter.rs            (pain run)
                                                              └─ ir_builder.rs ─ optimizations.rs ─ codegen.rs ─ llvm_tools.rs   (pain build)
```

| File | Responsibility |
| --- | --- |
| `src/lexer.rs`, `src/parser/mod.rs`, `src/ast.rs` | Tokens, indentation-based parser with error recovery, AST |
| `src/type_checker.rs`, `src/warnings.rs`, `src/error.rs` | Type checking, warnings, error formatting |
| `src/interpreter.rs` | Tree-walking interpreter, the reference implementation of the language |
| `src/stdlib.rs`, `src/stdlib_runtime.c` | Standard library signatures; C runtime linked into native executables |
| `src/ir.rs`, `src/ir_builder.rs` | SSA-style IR and lowering from the AST |
| `src/optimizations.rs` | IR passes (`Optimizer::optimize`) |
| `src/codegen.rs`, `src/mlir_codegen.rs` | LLVM IR text / MLIR text emission |
| `src/llvm_tools.rs` | Invoking `llc`, `clang` and the linker |
| `src/jit.rs`, `src/jit_orc.rs` | JIT, behind the `jit` feature (needs an LLVM 21 development install) |
| `src/pml*.rs` | PML configuration format: lexer, parser, values |
| `src/formatter.rs`, `src/doc_generator.rs` | `pain format`, `pain doc` |
| `src/modules.rs` | Multi-file programs: `import`, `pub`, loading the module graph and linking it into one `Program` |
| `src/query.rs` | `pain query`: call graph, callers/callees, code slices as JSON, across all files of the program |
| `src/main.rs` | CLI: `build`, `run`, `check`, `query`, `format`, `doc`, `repl`, PGO commands |

## Commands

From the workspace root:

```bash
cargo test -p pain-compiler                      # everything
cargo test -p pain-compiler --lib                # unit tests in src/
cargo test -p pain-compiler --test integration_test
cargo test -p pain-compiler --test full_pipeline_integration   # needs clang + llc on PATH
cargo clippy -p pain-compiler --all-targets -- -D warnings
cargo bench -p pain-compiler                     # criterion benches in benches/
```

## Asking the compiler instead of reading the file

```bash
pain query symbols --input file.pain              # every function/method: signature, lines
pain query callers NAME --input file.pain         # all call sites + call chains from the roots
pain query callees NAME --input file.pain         # direct calls and everything reachable
pain query slice NAME --input file.pain --text    # body + signatures it needs + who calls it
```

Methods are named `Class.method`; symbols of imported modules are `module.name`
(`combat.strike`, `combat.Unit.rest`), and a short name works when it is unique.
`callers` also returns `reachable_from`: everything that reaches the function,
directly or not. The answer carries `exact`: when it is `true`
the list is complete, because the language has no function values or dynamic
dispatch. Anything the graph cannot resolve (receiver of unknown type, call inside
an f-string) is listed in `notes` and turns `exact` off. Keep that guarantee when
adding language features: a new way to call code must either be resolved in
`query.rs` or reported there.

## Modules

- A module is a file. `import a.b: x, y` takes the `pub` items `x` and `y` from
  `a/b.pain`. This is the only import form: no wildcards, no aliases, no
  `module.name` access. Imports come before any definition.
- `pub fn` / `pub class` export an item; everything else is private to its file.
  Methods and fields have no visibility of their own.
- Module paths are resolved from the project root (`project_root`): the `src`
  directory of the package (`pain.toml`) the file is in, otherwise the directory
  of the entry file.
- The type checker, the interpreter and the backends still see one `Program`:
  `ModuleGraph::link` renames items of imported modules to `module.name` and
  rewrites every reference. The entry file keeps plain names (`main`).
- Module errors (`ModuleError`) are reported before type checking: missing file,
  unknown or private name, name clash, import cycle, name used without an import.
- Expressions carry no file. To tell which file a type error is in, the CLI type
  checks `link_isolated(i)` (bodies of all other modules removed) per module.

## Conventions

- New behaviour comes with a test. Unit tests sit in the module's `mod tests`;
  pipeline-level tests go to `tests/integration_test.rs` (no external tools) or
  `tests/full_pipeline_integration.rs` (compiles and runs a native executable).
- `TypeError` is large by design; functions returning `TypeResult` carry
  `#[allow(clippy::result_large_err)]`.
- A standard library function is registered in several places that must stay in
  sync: `get_stdlib_functions` and `is_stdlib_function` in `stdlib.rs`,
  `call_stdlib_function` in `interpreter.rs`, the `Intrinsic` arm in `codegen.rs`,
  and for native builds an implementation in `stdlib_runtime.c`.

## Things that are easy to get wrong

- **The native backend is narrower than the interpreter.** Code generation is
  effectively `i64`-only (loads, stores and phi nodes assume `i64`), `for` loops are
  a stub, and `stdlib_runtime.c` has no string functions (`len`, `concat`, ...), so
  such programs do not link. Check a feature in the interpreter first.
- **Mutable variables in the IR.** `IrBuilder` keeps a name → value map that is only
  valid for straight-line code. Variables assigned inside `if`/`while`/`for` bodies
  are lowered to stack slots (`Alloc` in the entry block, `Load`/`Store`). Do not
  bypass `read_var` / `write_var` when adding statements or expressions.
- **Optimizer passes are hand-written and conservative on purpose.** CSE is
  block-local and only for pure arithmetic; the inliner only splices single-block
  callees it can copy exactly; `loop_invariant_code_motion` and `loop_unrolling`
  are not run because `find_loops` has no dominator information. Re-enable them
  only together with a correct loop analysis and tests that execute the result.
- **Text-level checks are not enough for codegen.** LLVM IR that looks plausible
  can still be rejected by `llc` or compute the wrong result. Add an executing test
  and compare with the interpreter.
- **Interpreter objects are values.** `Value::Object` is cloned on read; a method
  call stores the receiver back into its variable. Aliasing two variables to one
  object is not supported.
- `main` returning an `int` prints the value and uses it as the process exit code,
  so a non-zero exit status of a compiled program is not an error by itself.
- `tests/property_tests.proptest-regressions` is written by proptest when a
  property fails; a saved seed makes the failure permanent. Look at it when a
  property test fails "for no reason".
