# Pain Benchmarks

Benchmark suite for the Pain programming language.

## Running Benchmarks

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench --bench fibonacci
cargo bench --bench sum
cargo bench --bench factorial
```

## Benchmark Results

Results are saved in `target/criterion/` directory. Open `target/criterion/*/index.html` in a browser to view detailed reports.

## Current Benchmarks

### Phase 1: Basic Infrastructure ✅
- **fibonacci**: Recursive Fibonacci calculation
- **sum**: Sum of numbers from 0 to n (using while loop)
- **factorial**: Recursive factorial calculation

### Phase 2: Numerical Benchmarks (Planned)
- Matrix multiplication
- Vector operations
- Basic linear algebra

### Phase 3: ML Benchmarks (Planned)
- Neural network forward pass
- Gradient computation
- Data preprocessing

### Phase 4: HPC Benchmarks (Planned)
- N-body simulation
- Stencil computations
- Advanced reductions

## Notes

- Currently benchmarks measure interpreter performance
- Compiled benchmarks (LLVM IR -> executable) will be added when compilation pipeline is complete
- Comparison with Python/Rust/C++ will be added in Phase 5

