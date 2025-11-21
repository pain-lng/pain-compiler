use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pain_compiler::pml_parser::parse_pml;

fn bench_parse_simple_map(c: &mut Criterion) {
    let source = "title: \"Hello\"\nwidth: 400\nheight: 300";
    c.bench_function("pml_parse_simple_map", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_nested_map(c: &mut Criterion) {
    let source = "window:\n\ttitle: \"Demo\"\n\twidth: 400\n\theight: 300\n\tlayout:\n\t\ttype: vbox\n\t\tspacing: 8";
    c.bench_function("pml_parse_nested_map", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_list(c: &mut Criterion) {
    let source =
        "items:\n\t- \"apple\"\n\t- \"banana\"\n\t- \"orange\"\n\t- \"grape\"\n\t- \"melon\"";
    c.bench_function("pml_parse_list", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_large_map(c: &mut Criterion) {
    let mut source = String::new();
    for i in 0..100 {
        source.push_str(&format!("key{}: \"value{}\"\n", i, i));
    }
    c.bench_function("pml_parse_large_map_100_keys", |b| {
        b.iter(|| parse_pml(black_box(&source)))
    });
}

fn bench_parse_large_list(c: &mut Criterion) {
    let mut source = String::from("items:\n");
    for i in 0..100 {
        source.push_str(&format!("\t- \"item{}\"\n", i));
    }
    c.bench_function("pml_parse_large_list_100_items", |b| {
        b.iter(|| parse_pml(black_box(&source)))
    });
}

fn bench_parse_deeply_nested(c: &mut Criterion) {
    let source = "a:\n\tb:\n\t\tc:\n\t\t\td:\n\t\t\t\te:\n\t\t\t\t\tf:\n\t\t\t\t\t\tg:\n\t\t\t\t\t\t\th:\n\t\t\t\t\t\t\t\ti:\n\t\t\t\t\t\t\t\t\tj:\n\t\t\t\t\t\t\t\t\t\tk: \"value\"";
    c.bench_function("pml_parse_deeply_nested_10_levels", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_complex_ui(c: &mut Criterion) {
    let source = r#"window:
	id: main_window
	title: "Pain Demo"
	width: 400
	height: 300
	layout:
		type: vbox
		spacing: 8
		padding: 12
		children:
			- type: label
			  id: label_counter
			  text: "Clicks: 0"
			- type: button
			  id: button_click
			  text: "Click me"
			  on_click: on_btn_click
			- type: input
			  id: input_name
			  placeholder: "Enter name"
			  required: true"#;
    c.bench_function("pml_parse_complex_ui", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_with_comments(c: &mut Criterion) {
    let source = r#"# Application configuration
app:
	name: "My App"  # Application name
	version: "1.0.0"
	# Debug mode
	debug: false
	port: 8080  # Server port"#;
    c.bench_function("pml_parse_with_comments", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_unicode(c: &mut Criterion) {
    let source =
        "russian: \"Привет\"\njapanese: \"こんにちは\"\nemoji: \"😀🎉\"\nchinese: \"你好\"";
    c.bench_function("pml_parse_unicode", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

fn bench_parse_escape_sequences(c: &mut Criterion) {
    let source = r#"message: "Line1\nLine2\tTabbed\rReturn\\Backslash\"Quote""#;
    c.bench_function("pml_parse_escape_sequences", |b| {
        b.iter(|| parse_pml(black_box(source)))
    });
}

criterion_group!(
    benches,
    bench_parse_simple_map,
    bench_parse_nested_map,
    bench_parse_list,
    bench_parse_large_map,
    bench_parse_large_list,
    bench_parse_deeply_nested,
    bench_parse_complex_ui,
    bench_parse_with_comments,
    bench_parse_unicode,
    bench_parse_escape_sequences
);
criterion_main!(benches);
