use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pain_compiler::{parse, type_check_program, CodeGenerator, IrBuilder, Optimizer};

// TODO: Add interpreter benchmarks when interpreter return values are supported
// fn fibonacci_interpreter(n: i64) -> i64 { ... }

#[allow(dead_code)]
fn fibonacci_compiled(n: i64) -> i64 {
    let source = format!(
        r#"
fn fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

fn main() -> int:
    return fib({})
"#,
        n
    );

    let program = parse(&source).unwrap();
    type_check_program(&program).unwrap();

    let ir_builder = IrBuilder::new();
    let mut ir = ir_builder.build(&program);
    ir = Optimizer::optimize(ir);

    let codegen = CodeGenerator::new(ir);
    let _llvm_ir = codegen.generate();

    // TODO: Compile LLVM IR to executable and run
    0 // Placeholder
}

fn benchmark_fibonacci(c: &mut Criterion) {
    let mut group = c.benchmark_group("fibonacci");

    // Benchmark IR generation and optimization
    group.bench_function("ir_gen_n10", |b| {
        b.iter(|| {
            let source = r#"
fn fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

fn main() -> int:
    return fib(10)
"#
            .to_string();
            let program = parse(&source).unwrap();
            type_check_program(&program).unwrap();
            let ir_builder = IrBuilder::new();
            let mut ir = ir_builder.build(&program);
            ir = Optimizer::optimize(ir);
            black_box(ir)
        })
    });

    // Benchmark codegen
    group.bench_function("codegen_n10", |b| {
        b.iter(|| {
            let source = r#"
fn fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

fn main() -> int:
    return fib(10)
"#
            .to_string();
            let program = parse(&source).unwrap();
            type_check_program(&program).unwrap();
            let ir_builder = IrBuilder::new();
            let mut ir = ir_builder.build(&program);
            ir = Optimizer::optimize(ir);
            let codegen = CodeGenerator::new(ir);
            black_box(codegen.generate())
        })
    });

    group.finish();
}

criterion_group!(benches, benchmark_fibonacci);
criterion_main!(benches);
