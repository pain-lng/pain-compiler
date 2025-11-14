use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pain_compiler::{parse, type_check_program, IrBuilder, CodeGenerator, Optimizer};

fn benchmark_sum(c: &mut Criterion) {
    let mut group = c.benchmark_group("sum");
    
    // Benchmark IR generation and optimization
    group.bench_function("ir_gen_n1000", |b| {
        b.iter(|| {
            let source = r#"
fn sum(n: int) -> int:
    var result = 0
    var i = 0
    while i <= n:
        result = result + i
        i = i + 1
    return result

fn main() -> int:
    return sum(1000)
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
    group.bench_function("codegen_n1000", |b| {
        b.iter(|| {
            let source = r#"
fn sum(n: int) -> int:
    var result = 0
    var i = 0
    while i <= n:
        result = result + i
        i = i + 1
    return result

fn main() -> int:
    return sum(1000)
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

criterion_group!(benches, benchmark_sum);
criterion_main!(benches);

