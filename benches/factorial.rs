use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pain_compiler::{parse, type_check_program, IrBuilder, CodeGenerator, Optimizer};

fn benchmark_factorial(c: &mut Criterion) {
    let mut group = c.benchmark_group("factorial");
    
    // Benchmark IR generation and optimization
    group.bench_function("ir_gen_n10", |b| {
        b.iter(|| {
            let source = r#"
fn fact(n: int) -> int:
    if n <= 1:
        return 1
    return n * fact(n - 1)

fn main() -> int:
    return fact(10)
"#;
            let program = parse(source).unwrap();
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
fn fact(n: int) -> int:
    if n <= 1:
        return 1
    return n * fact(n - 1)

fn main() -> int:
    return fact(10)
"#;
            let program = parse(source).unwrap();
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

criterion_group!(benches, benchmark_factorial);
criterion_main!(benches);

