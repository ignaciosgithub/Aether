use aether_frontend::parse_source;
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[test]
fn x86_64_linux_emits_f32_return() {
    let src = r#"
        pub func main() -> f32 {
            return 1.5;
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("movss") || asm.contains("movd") || asm.contains(".long") || asm.contains("movsd"), "expected float handling");
}

#[test]
fn x86_64_linux_emits_abs_i64_function() {
    let src = r#"
        pub func main() -> i64 {
            return abs_i64(-5);
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("abs_i64:"), "expected abs_i64 function to be emitted");
    assert!(asm.contains("call abs_i64") || asm.contains("callq abs_i64"), "expected call to abs_i64");
}

#[test]
fn x86_64_linux_emits_min_i64_function() {
    let src = r#"
        pub func main() -> i64 {
            return min_i64(3, 5);
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("min_i64:"), "expected min_i64 function to be emitted");
    assert!(asm.contains("call min_i64") || asm.contains("callq min_i64"), "expected call to min_i64");
}

#[test]
fn x86_64_linux_emits_max_i64_function() {
    let src = r#"
        pub func main() -> i64 {
            return max_i64(3, 5);
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("max_i64:"), "expected max_i64 function to be emitted");
    assert!(asm.contains("call max_i64") || asm.contains("callq max_i64"), "expected call to max_i64");
}

#[test]
fn x86_64_linux_emits_sqrt_f64_function() {
    let src = r#"
        pub func main() -> f64 {
            return sqrt_f64(4.0);
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("sqrt_f64:"), "expected sqrt_f64 function to be emitted");
    assert!(asm.contains("sqrtsd"), "expected sqrtsd instruction in sqrt_f64");
}

#[test]
fn x86_64_linux_emits_sqrt_f32_function() {
    let src = r#"
        pub func main() -> f32 {
            return sqrt_f32(4.0);
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("sqrt_f32:"), "expected sqrt_f32 function to be emitted");
    assert!(asm.contains("sqrtss"), "expected sqrtss instruction in sqrt_f32");
}

#[test]
fn x86_64_linux_emits_str_len_function() {
    let src = r#"
        pub func main() -> i64 {
            return str_len("hello");
        }
    "#;
    let module = parse_source(src).expect("parse ok");
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module).expect("codegen ok");
    assert!(asm.contains("str_len:"), "expected str_len function to be emitted");
}
