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
fn if_true_or_false_inlining_ok() {
    let class_txt = "Foo = ( run = (
        true ifTrue: [ ^true ].
        ^ false
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushGlobal(0),
            JumpOnFalseTopNil(3),
            PushGlobal(0),
            ReturnNonLocal,
            Pop,
            PushGlobal(1),
            ReturnNonLocal,
            Pop,
            PushArgument(0, 0),
            ReturnLocal,
        ],
    );

    let class_txt2 = "Foo = ( run = (
        false ifFalse: [ ^false ].
        ^ true
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushGlobal(0),
            JumpOnTrueTopNil(3),
            PushGlobal(0),
            ReturnNonLocal,
            Pop,
            PushGlobal(1),
            ReturnNonLocal,
            Pop,
            PushArgument(0, 0),
            ReturnLocal,
        ],
    );
}

#[test]
fn if_true_if_false_inlining_ok() {
    let class_txt = "Foo = ( run = ( true ifTrue: [ ^true ] ifFalse: [ ^false]. ))";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushGlobal(0),
            JumpOnFalsePop(4),
            PushGlobal(0),
            ReturnNonLocal,
            Jump(3),
            PushGlobal(1),
            ReturnNonLocal,
            Pop,
            PushArgument(0, 0),
            ReturnLocal,
        ],
    );

    let class_txt2 = "Foo = ( run = ( true ifFalse: [ ^false ] ifTrue: [ ^ true]. ))";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");

    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushGlobal(0),
            JumpOnTruePop(4),
            PushGlobal(1),
            ReturnNonLocal,
            Jump(3),
            PushGlobal(0),
            ReturnNonLocal,
            Pop,
            PushArgument(0, 0),
            ReturnLocal,
        ],
    );
}

#[test]
fn while_true_false_inlining_ok() {
    let class_txt = "Foo = ( run = (
        | cnt |
        cnt := 42.
        [ cnt < 1000000 ] whileTrue: [
            cnt := cnt + 1.
        ]
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    dbg!(&bytecodes);
    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushLocal(0, 0),
            PushConstant1,
            Send2(2),
            JumpOnFalsePop(6),
            PushLocal(0, 0),
            Push1,
            Send2(3),
            PopLocal(0, 0),
            JumpBackward(8),
        ],
    );

    // let class_txt_2 = class_txt.replace("whileTrue", "whileFalse");
    // let bytecodes = get_bytecodes_from_method(class_txt_2.as_str(), "run");
    //
    // expect_bytecode_sequence(&bytecodes, &[
    //     JumpOnTruePop(8),
    //     PushLocal(0, 0),
    //     PushConstant(3),
    //     Send2(4),
    //     Dup,
    //     PopLocal(0, 0),
    //     Pop,
    //     JumpBackward(10)
    // ])
}

#[test]
fn or_and_inlining_ok() {
    let class_txt = "Foo = ( run = (
        ^ (true or: [ false ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushGlobal(0),
            JumpOnTruePop(3),
            PushGlobal(1),
            Jump(2),
            PushGlobal(0),
            ReturnNonLocal,
        ],
    );

    let class_txt2 = "Foo = ( run = (
        ^ (true and: [ false ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");
    expect_bytecode_sequence(
        &bytecodes,
        &[
            PushGlobal(0),
            JumpOnFalsePop(3),
            PushGlobal(1),
            Jump(2),
            PushGlobal(1),
            ReturnNonLocal,
        ],
    );
}

#[test]
fn inlining_pyramid() {
    let class_txt = "Foo = ( run = (
        | a b c d e f g |
        ^ (a ifTrue: [b ifTrue: [c ifTrue: [d ifTrue: [e ifTrue: [f ifTrue: [g]]]]]])
    ))
    ";

    let class_txt2 = "Foo = ( run = (
        | a |
        ^ (a ifTrue: [| b | b ifTrue: [| c | c ifTrue: [| d | d ifTrue: [| e | e ifTrue: [| f | f ifTrue: [| g | g]]]]]])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    let bytecodes2 = get_bytecodes_from_method(class_txt2, "run");

    let expected_bc = &[
        PushLocal(0, 0),
        JumpOnFalseTopNil(12),
        PushLocal(0, 1),
        JumpOnFalseTopNil(10),
        PushLocal(0, 2),
        JumpOnFalseTopNil(8),
        PushLocal(0, 3),
        JumpOnFalseTopNil(6),
        PushLocal(0, 4),
        JumpOnFalseTopNil(4),
        PushLocal(0, 5),
        JumpOnFalseTopNil(2),
        PushLocal(0, 6),
        ReturnNonLocal,
    ];

    expect_bytecode_sequence(&bytecodes, expected_bc);
    expect_bytecode_sequence(&bytecodes2, expected_bc);
}
