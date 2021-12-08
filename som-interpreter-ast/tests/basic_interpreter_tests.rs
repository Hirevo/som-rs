use std::path::PathBuf;

use som_interpreter_ast::evaluate::Evaluate;
use som_interpreter_ast::frame::FrameKind;
use som_interpreter_ast::invokable::Return;
use som_interpreter_ast::universe::Universe;
use som_interpreter_ast::value::Value;
use som_lexer::{Lexer, Token};

use som_parser::lang;

fn setup_universe() -> Universe {
    let classpath = vec![
        PathBuf::from("../core-lib/Smalltalk"),
        PathBuf::from("../core-lib/TestSuite/BasicInterpreterTests"),
    ];
    Universe::with_classpath(classpath).expect("could not setup test universe")
}

#[test]
fn basic_interpreter_tests() {
    let mut universe = setup_universe();

    let return_class = Value::Class(universe.load_class("Return").unwrap());
    let compiler_simplification_class =
        Value::Class(universe.load_class("CompilerSimplification").unwrap());

    let tests: &[(&str, Value)] = &[
        // {"Self", "assignSuper", 42, ProgramDefinitionError.class},
        ("MethodCall test", Value::Integer(42)),
        ("MethodCall test2", Value::Integer(42)),
        ("NonLocalReturn test1", Value::Integer(42)),
        ("NonLocalReturn test2", Value::Integer(43)),
        ("NonLocalReturn test3", Value::Integer(3)),
        ("NonLocalReturn test4", Value::Integer(42)),
        ("NonLocalReturn test5", Value::Integer(22)),
        ("Blocks testArg1", Value::Integer(42)),
        ("Blocks testArg2", Value::Integer(77)),
        ("Blocks testArgAndLocal", Value::Integer(8)),
        ("Blocks testArgAndContext", Value::Integer(8)),
        ("Blocks testEmptyZeroArg", Value::Integer(1)),
        ("Blocks testEmptyOneArg", Value::Integer(1)),
        ("Blocks testEmptyTwoArg", Value::Integer(1)),
        ("Return testReturnSelf", return_class.clone()),
        ("Return testReturnSelfImplicitly", return_class.clone()),
        ("Return testNoReturnReturnsSelf", return_class.clone()),
        (
            "Return testBlockReturnsImplicitlyLastValue",
            Value::Integer(4),
        ),
        ("IfTrueIfFalse test", Value::Integer(42)),
        ("IfTrueIfFalse test2", Value::Integer(33)),
        ("IfTrueIfFalse test3", Value::Integer(4)),
        (
            "CompilerSimplification testReturnConstantSymbol",
            Value::Symbol(universe.intern_symbol("constant")),
        ),
        (
            "IfTrueIfFalse testIfTrueTrueResult",
            Value::Class(universe.integer_class()),
        ),
        (
            "IfTrueIfFalse testIfTrueFalseResult",
            Value::Class(universe.nil_class()),
        ),
        (
            "IfTrueIfFalse testIfFalseTrueResult",
            Value::Class(universe.nil_class()),
        ),
        (
            "IfTrueIfFalse testIfFalseFalseResult",
            Value::Class(universe.integer_class()),
        ),
        (
            "CompilerSimplification testReturnConstantInt",
            Value::Integer(42),
        ),
        (
            "CompilerSimplification testReturnSelf",
            compiler_simplification_class.clone(),
        ),
        (
            "CompilerSimplification testReturnSelfImplicitly",
            compiler_simplification_class.clone(),
        ),
        (
            "CompilerSimplification testReturnArgumentN",
            Value::Integer(55),
        ),
        (
            "CompilerSimplification testReturnArgumentA",
            Value::Integer(44),
        ),
        (
            "CompilerSimplification testSetField",
            Value::Symbol(universe.intern_symbol("foo")),
        ),
        ("CompilerSimplification testGetField", Value::Integer(40)),
        ("Hash testHash", Value::Integer(444)),
        ("Arrays testEmptyToInts", Value::Integer(3)),
        ("Arrays testPutAllInt", Value::Integer(5)),
        ("Arrays testPutAllNil", Value::Class(universe.nil_class())),
        ("Arrays testPutAllBlock", Value::Integer(3)),
        ("Arrays testNewWithAll", Value::Integer(1)),
        ("BlockInlining testNoInlining", Value::Integer(1)),
        ("BlockInlining testOneLevelInlining", Value::Integer(1)),
        (
            "BlockInlining testOneLevelInliningWithLocalShadowTrue",
            Value::Integer(2),
        ),
        (
            "BlockInlining testOneLevelInliningWithLocalShadowFalse",
            Value::Integer(1),
        ),
        (
            "BlockInlining testShadowDoesntStoreWrongLocal",
            Value::Integer(33),
        ),
        (
            "BlockInlining testShadowDoesntReadUnrelated",
            Value::Class(universe.nil_class()),
        ),
        ("BlockInlining testBlockNestedInIfTrue", Value::Integer(2)),
        ("BlockInlining testBlockNestedInIfFalse", Value::Integer(42)),
        ("BlockInlining testStackDisciplineTrue", Value::Integer(1)),
        ("BlockInlining testStackDisciplineFalse", Value::Integer(2)),
        (
            "BlockInlining testDeepNestedInlinedIfTrue",
            Value::Integer(3),
        ),
        (
            "BlockInlining testDeepNestedInlinedIfFalse",
            Value::Integer(42),
        ),
        (
            "BlockInlining testDeepNestedBlocksInInlinedIfTrue",
            Value::Integer(5),
        ),
        (
            "BlockInlining testDeepNestedBlocksInInlinedIfFalse",
            Value::Integer(43),
        ),
        ("BlockInlining testDeepDeepNestedTrue", Value::Integer(9)),
        ("BlockInlining testDeepDeepNestedFalse", Value::Integer(43)),
        ("BlockInlining testToDoNestDoNestIfTrue", Value::Integer(2)),
        ("NonLocalVars testWriteDifferentTypes", Value::Double(3.75)),
        ("ObjectCreation test", Value::Integer(1000000)),
        ("Regressions testSymbolEquality", Value::Integer(1)),
        ("Regressions testSymbolReferenceEquality", Value::Integer(1)),
        ("Regressions testUninitializedLocal", Value::Integer(1)),
        (
            "Regressions testUninitializedLocalInBlock",
            Value::Integer(1),
        ),
        ("BinaryOperation test", Value::Integer(3 + 8)),
        ("NumberOfTests numberOfTests", Value::Integer(65)),
    ];

    for (expr, expected) in tests {
        println!("testing: '{}'", expr);

        let mut lexer = Lexer::new(expr).skip_comments(true).skip_whitespace(true);
        let tokens: Vec<Token> = lexer.by_ref().collect();
        assert!(
            lexer.text().is_empty(),
            "could not fully tokenize test expression"
        );

        let ast = som_parser::apply(lang::expression(), tokens.as_slice()).unwrap();

        let signature = universe.intern_symbol(expr.split(' ').skip(1).next().unwrap_or("unknown"));

        let kind = FrameKind::Method {
            signature,
            holder: universe.system_class(),
            self_value: Value::System,
        };
        let output = universe.with_frame(kind, |universe| ast.evaluate(universe));

        match &output {
            Return::Local(output) => assert_eq!(output, expected, "unexpected test output value"),
            Return::NonLocal(_, _) => {
                panic!("unexpected non-local return from basic interpreter test")
            }
            Return::Restart => panic!("unexpected `restart` from basic interpreter test"),
            Return::Exception(err) => panic!("unexpected exception: '{}'", err),
        }
    }
}
