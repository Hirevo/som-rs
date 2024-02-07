use std::path::PathBuf;
use som_core::bytecode::Bytecode;

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

    let mut lexer = Lexer::new(class_txt).skip_comments(true).skip_whitespace(true);
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
        _ => unreachable!()
    }
}

fn expect_bytecode_sequence(bytecodes: &Vec<Bytecode>, expected_bc_sequence: &[Bytecode]) {
    assert!(bytecodes.windows(expected_bc_sequence.len()).any(|window| window == expected_bc_sequence))
}

#[test]
fn if_true_or_false_inlining_ok() {
    let class_txt = "Foo = ( run = (
        true ifTrue: [ ^true ].
        ^ false
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::PushGlobal(0),
        Bytecode::JumpOnFalseTopNil(3),
        Bytecode::PushGlobal(0),
        Bytecode::ReturnNonLocal,
        Bytecode::Pop,
        Bytecode::PushGlobal(1),
        Bytecode::ReturnNonLocal,
        Bytecode::Pop,
        Bytecode::PushArgument(0, 0),
        Bytecode::ReturnLocal
    ]);

    let class_txt2 = "Foo = ( run = (
        false ifFalse: [ ^false ].
        ^ true
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");

    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::PushGlobal(0),
        Bytecode::JumpOnTrueTopNil(3),
        Bytecode::PushGlobal(0),
        Bytecode::ReturnNonLocal,
        Bytecode::Pop,
        Bytecode::PushGlobal(1),
        Bytecode::ReturnNonLocal,
        Bytecode::Pop,
        Bytecode::PushArgument(0, 0),
        Bytecode::ReturnLocal
    ]);
}

#[test]
fn if_true_if_false_inlining_ok() {
    let class_txt = "Foo = ( run = ( true ifTrue: [ ^true ] ifFalse: [ ^false]. ))";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::PushGlobal(0),
        Bytecode::JumpOnFalsePop(4),
        Bytecode::PushGlobal(0),
        Bytecode::ReturnNonLocal,
        Bytecode::Jump(3),
        Bytecode::PushGlobal(1),
        Bytecode::ReturnNonLocal,
        Bytecode::Pop,
        Bytecode::PushArgument(0, 0),
        Bytecode::ReturnLocal,
    ]);

    let class_txt2 = "Foo = ( run = ( true ifFalse: [ ^false ] ifTrue: [ ^ true]. ))";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");

    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::PushGlobal(0),
        Bytecode::JumpOnTruePop(4),
        Bytecode::PushGlobal(1),
        Bytecode::ReturnNonLocal,
        Bytecode::Jump(3),
        Bytecode::PushGlobal(0),
        Bytecode::ReturnNonLocal,
        Bytecode::Pop,
        Bytecode::PushArgument(0, 0),
        Bytecode::ReturnLocal,
    ]);
}

#[test]
fn while_true_false_inlining_ok() {
    let class_txt = "Foo = ( run = (
        | cnt |
        cnt := 0.
        [ cnt < 1000000 ] whileTrue: [
            cnt := cnt + 1.
        ]
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");

    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::JumpOnFalsePop(8),
        Bytecode::PushLocal(0, 0),
        Bytecode::PushConstant(3),
        Bytecode::Send(4),
        Bytecode::Dup,
        Bytecode::PopLocal(0, 0),
        Bytecode::Pop,
        Bytecode::JumpBackward(10)
    ]);

    let class_txt_2 = class_txt.replace("whileTrue", "whileFalse");
    let bytecodes = get_bytecodes_from_method(class_txt_2.as_str(), "run");

    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::JumpOnTruePop(8),
        Bytecode::PushLocal(0, 0),
        Bytecode::PushConstant(3),
        Bytecode::Send(4),
        Bytecode::Dup,
        Bytecode::PopLocal(0, 0),
        Bytecode::Pop,
        Bytecode::JumpBackward(10)
    ])
}

#[test]
fn or_and_inlining_ok() {
    let class_txt = "Foo = ( run = (
        ^ (true or: [ false ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt, "run");
    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::PushGlobal(0),
        Bytecode::JumpOnTruePop(3),
        Bytecode::PushGlobal(1),
        Bytecode::Jump(2),
        Bytecode::PushGlobal(0),
        Bytecode::ReturnNonLocal
    ]);

    let class_txt2 = "Foo = ( run = (
        ^ (true and: [ false ])
    ))
    ";

    let bytecodes = get_bytecodes_from_method(class_txt2, "run");
    expect_bytecode_sequence(&bytecodes, &[
        Bytecode::PushGlobal(0),
        Bytecode::JumpOnFalsePop(3),
        Bytecode::PushGlobal(1),
        Bytecode::Jump(2),
        Bytecode::PushGlobal(1),
        Bytecode::ReturnNonLocal
    ]);
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
        Bytecode::PushLocal(0, 0),
        Bytecode::JumpOnFalseTopNil(12),
        Bytecode::PushLocal(0, 1),
        Bytecode::JumpOnFalseTopNil(10),
        Bytecode::PushLocal(0, 2),
        Bytecode::JumpOnFalseTopNil(8),
        Bytecode::PushLocal(0, 3),
        Bytecode::JumpOnFalseTopNil(6),
        Bytecode::PushLocal(0, 4),
        Bytecode::JumpOnFalseTopNil(4),
        Bytecode::PushLocal(0, 5),
        Bytecode::JumpOnFalseTopNil(2),
        Bytecode::PushLocal(0, 6),
        Bytecode::ReturnNonLocal
    ];

    expect_bytecode_sequence(&bytecodes, expected_bc);
    expect_bytecode_sequence(&bytecodes2, expected_bc);
}