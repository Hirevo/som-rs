use som_core::bytecode::Bytecode;
use som_core::bytecode::Bytecode::*;
use std::path::PathBuf;

use som_interpreter_bc::compiler;
use som_interpreter_bc::method::MethodKind;
use som_interpreter_bc::universe::Universe;
use som_lexer::{Lexer, Token};
use som_parser::lang;

fn setup_universe() -> Universe {
    let classpath = vec![
        PathBuf::from("../core-lib/Smalltalk"),
        PathBuf::from("../core-lib/TestSuite/BasicInterpreterTests"),
    ];
    Universe::with_classpath(classpath).expect("could not setup test universe")
}

fn get_bytecodes_from_method(class_txt: &str, method_name: &str) -> Vec<Bytecode> {
    let mut universe = setup_universe();

    let method_name_interned = universe.intern_symbol(method_name);

    let mut lexer = Lexer::new(class_txt)
        .skip_comments(true)
        .skip_whitespace(true);
    let tokens: Vec<Token> = lexer.by_ref().collect();
    assert!(
        lexer.text().is_empty(),
        "could not fully tokenize test expression"
    );

    let class_def = som_parser::apply(lang::class_def(), tokens.as_slice()).unwrap();

    let object_class = universe.object_class();
    let class = compiler::compile_class(&mut universe.interner, &class_def, Some(&object_class));
    assert!(class.is_some(), "could not compile test expression");

    let class = class.unwrap();
    let method = class
        .borrow()
        .lookup_method(method_name_interned)
        .expect("method not found ??");

    match &method.as_ref().kind {
        MethodKind::Defined(m) => m.body.clone(),
        _ => unreachable!(),
    }
}

fn expect_bytecode_sequence(bytecodes: &Vec<Bytecode>, expected_bc_sequence: &[Bytecode]) {
    assert!(bytecodes
        .windows(expected_bc_sequence.len())
        .any(|window| window == expected_bc_sequence))
}

#[test]
fn push_0_1_nil_bytecodes() {
    let class_txt = "Foo = ( run = (
        | a b c |
        a := 0.
        b := 1.
        c := nil.
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            Push0,
            PopLocal(0, 0),
            Push1,
            PopLocal(0, 1),
            PushNil,
            PopLocal(0, 2),
        ],
    );
}

#[test]
fn push_constant_bytecodes() {
    let class_txt = "Foo = ( run = (
        | a b c d e f |
        a := 'abc'.
        b := 'def'.
        c := 'ghi'.
        d := 'abc'.
        e := 'def'.
        f := 'ghi'.
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushConstant0,
            PopLocal(0, 0),
            PushConstant1,
            PopLocal(0, 1),
            PushConstant2,
            PopLocal(0, 2),
            PushConstant0,
            PopLocal(0, 3),
            PushConstant1,
            PopLocal(0, 4),
            PushConstant2,
            PopLocal(0, 5),
        ],
    );
}

#[test]
fn send_bytecodes() {
    let class_txt = "Foo = (
        send: a three: b = (
            ^ false
        )

        send: a with: b four: c = (
            ^ false
        )

        run = (
            1 abs.
            1 + 1.
            self send: 1 three: 1.
            self send: 1 with: 1 four: 1.
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(&bytecodes, &[Push1, Send1(0)]);

    expect_bytecode_sequence(&bytecodes, &[Push1, Push1, Send2(1)]);

    expect_bytecode_sequence(&bytecodes, &[PushArgument(0, 0), Push1, Push1, Send3(2)]);

    expect_bytecode_sequence(
        &bytecodes,
        &[PushArgument(0, 0), Push1, Push1, Push1, SendN(3)],
    );
}

#[test]
fn super_send_bytecodes() {
    let class_txt = "Foo = (
        run = (
            super send1.
            super sendtwo: 1.
            super send: 1 three: 1.
            super send: 1 with: 1 four: 1.
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(&bytecodes, &[PushArgument(0, 0), SuperSend1(0)]);

    expect_bytecode_sequence(&bytecodes, &[PushArgument(0, 0), Push1, SuperSend2(1)]);

    expect_bytecode_sequence(
        &bytecodes,
        &[PushArgument(0, 0), Push1, Push1, SuperSend3(2)],
    );

    expect_bytecode_sequence(
        &bytecodes,
        &[PushArgument(0, 0), Push1, Push1, Push1, SuperSendN(3)],
    );
}

#[ignore]
#[test]
fn something_jump_bug_popx() {
    // todo: this test is about jump BC pointing to redundant dup/popx/pop sequences...
    // ...therefore breaking when they're optimized and the jump doesn't know what to do.
    // this issue is currently being circumvented by straight up not removing the sequence when it's a jump target.
    // but this needs to be changed in the future. there's likely an underlying issue that this test right there exemplifies?

    let class_txt = "Foo = (
        testIfTrueTrueResult = (
          | result |
          result := true ifTrue: [ 1 ].
          ^ result class
        )
    )
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "testIfTrueTrueResult");
    dbg!(&bytecodes);

    let _bc_no_removal = &[
        PushGlobal(0),
        JumpOnFalseTopNil(2),
        Push1,
        Dup,
        PopLocal(0, 0),
        Pop,
        PushLocal(0, 0),
        Send1(1),
        ReturnNonLocal,
        Pop,
        PushArgument(0, 0),
        ReturnLocal,
    ];

    let expected_bytecodes: &[Bytecode] = &[
        PushGlobal(0),
        JumpOnFalseTopNil(2),
        Push1,
        PopLocal(0, 0),
        PushLocal(0, 0),
        Send1(1),
        ReturnNonLocal,
        Pop,
        PushArgument(0, 0),
        ReturnLocal,
    ];

    expect_bytecode_sequence(&bytecodes, expected_bytecodes);
}
