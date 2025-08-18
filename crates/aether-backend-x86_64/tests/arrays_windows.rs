use aether_backend_x86_64::X86_64LinuxCodegen;

#[test]
#[cfg(target_os = "windows")]
fn win_nonmain_array_index_bounds_and_load() {
    let src = r#"
        struct Dummy { x: i32 }
        func foo() -> i32 {
            let xs: [i32; 3] = [10,20,30];
            let i: i32 = 1;
            println(xs[i]);
            return 0;
        }
    "#;
    let module = aether_frontend::parser::Parser::parse(&aether_frontend::lexer::Lexer::tokenize(src).unwrap()).unwrap();
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module);
    assert!(asm.contains("cmp ecx, 3"), "must compare index against length");
    assert!(asm.contains("jae OOBI_"), "must branch to OOB on >= len");
    assert!(asm.contains("lea rax, [rbp-"), "must take base address of array");
    assert!(asm.contains("lea rax, [rax+rcx*4]") || asm.contains("lea rax, [rax + rcx*4]"), "must scale index by 4");
}

#[test]
#[cfg(target_os = "windows")]
fn win_nonmain_array_assign_index() {
    let src = r#"
        func foo() -> i32 {
            let xs: [i64; 2] = [1,2];
            let i: i32 = 1;
            xs[i] = 5;
            return 0;
        }
    "#;
    let module = aether_frontend::parser::Parser::parse(&aether_frontend::lexer::Lexer::tokenize(src).unwrap()).unwrap();
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&module);
    assert!(asm.contains("cmp ecx, 2"), "must compare index against length");
    assert!(asm.contains("lea rax, [rax+rcx*8]") || asm.contains("lea rax, [rax + rcx*8]"), "must scale index by 8");
    assert!(asm.contains("mov qword ptr [rax], rdx") || asm.contains("mov qword ptr [rax],"), "must store 64-bit value");
}
