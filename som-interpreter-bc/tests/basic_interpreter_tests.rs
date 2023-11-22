use std::path::PathBuf;

use som_gc::GcHeap;
use som_interpreter_bc::compiler;
use som_interpreter_bc::frame::FrameKind;
use som_interpreter_bc::interpreter::Interpreter;
use som_interpreter_bc::universe::Universe;
use som_interpreter_bc::value::Value;
use som_lexer::{Lexer, Token};
use som_parser::lang;

fn setup_universe(heap: &mut GcHeap) -> Universe {
    let classpath = vec![
        PathBuf::from("../core-lib/Smalltalk"),
        PathBuf::from("../core-lib/TestSuite/BasicInterpreterTests"),
    ];
    Universe::with_classpath(heap, classpath).expect("could not setup test universe")
}

#[test]
fn basic_interpreter_tests() {
    let mut heap = GcHeap::new();

    let mut universe = setup_universe(&mut heap);

    let return_class = Value::new_class(&universe.load_class(&mut heap, "Return").unwrap());
    let compiler_simplification_class = Value::new_class(
        &universe
            .load_class(&mut heap, "CompilerSimplification")
            .unwrap(),
    );

    let method_name = universe.intern_symbol("run");

    let tests: &[(&str, Value)] = &[
        // {"Self", "assignSuper", 42, ProgramDefinitionError.class},
        ("MethodCall test", Value::new_integer(42)),
        ("MethodCall test2", Value::new_integer(42)),
        ("NonLocalReturn test1", Value::new_integer(42)),
        ("NonLocalReturn test2", Value::new_integer(43)),
        ("NonLocalReturn test3", Value::new_integer(3)),
        ("NonLocalReturn test4", Value::new_integer(42)),
        ("NonLocalReturn test5", Value::new_integer(22)),
        ("Blocks testArg1", Value::new_integer(42)),
        ("Blocks testArg2", Value::new_integer(77)),
        ("Blocks testArgAndLocal", Value::new_integer(8)),
        ("Blocks testArgAndContext", Value::new_integer(8)),
        ("Blocks testEmptyZeroArg", Value::new_integer(1)),
        ("Blocks testEmptyOneArg", Value::new_integer(1)),
        ("Blocks testEmptyTwoArg", Value::new_integer(1)),
        ("Return testReturnSelf", return_class.clone()),
        ("Return testReturnSelfImplicitly", return_class.clone()),
        ("Return testNoReturnReturnsSelf", return_class.clone()),
        (
            "Return testBlockReturnsImplicitlyLastValue",
            Value::new_integer(4),
        ),
        ("IfTrueIfFalse test", Value::new_integer(42)),
        ("IfTrueIfFalse test2", Value::new_integer(33)),
        ("IfTrueIfFalse test3", Value::new_integer(4)),
        (
            "CompilerSimplification testReturnConstantSymbol",
            Value::new_symbol(universe.intern_symbol("constant")),
        ),
        (
            "IfTrueIfFalse testIfTrueTrueResult",
            Value::new_class(&universe.integer_class()),
        ),
        (
            "IfTrueIfFalse testIfTrueFalseResult",
            Value::new_class(&universe.nil_class()),
        ),
        (
            "IfTrueIfFalse testIfFalseTrueResult",
            Value::new_class(&universe.nil_class()),
        ),
        (
            "IfTrueIfFalse testIfFalseFalseResult",
            Value::new_class(&universe.integer_class()),
        ),
        (
            "CompilerSimplification testReturnConstantInt",
            Value::new_integer(42),
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
            Value::new_integer(55),
        ),
        (
            "CompilerSimplification testReturnArgumentA",
            Value::new_integer(44),
        ),
        (
            "CompilerSimplification testSetField",
            Value::new_symbol(universe.intern_symbol("foo")),
        ),
        (
            "CompilerSimplification testGetField",
            Value::new_integer(40),
        ),
        ("Hash testHash", Value::new_integer(444)),
        ("Arrays testEmptyToInts", Value::new_integer(3)),
        ("Arrays testPutAllInt", Value::new_integer(5)),
        (
            "Arrays testPutAllNil",
            Value::new_class(&universe.nil_class()),
        ),
        ("Arrays testPutAllBlock", Value::new_integer(3)),
        ("Arrays testNewWithAll", Value::new_integer(1)),
        ("BlockInlining testNoInlining", Value::new_integer(1)),
        ("BlockInlining testOneLevelInlining", Value::new_integer(1)),
        (
            "BlockInlining testOneLevelInliningWithLocalShadowTrue",
            Value::new_integer(2),
        ),
        (
            "BlockInlining testOneLevelInliningWithLocalShadowFalse",
            Value::new_integer(1),
        ),
        (
            "BlockInlining testShadowDoesntStoreWrongLocal",
            Value::new_integer(33),
        ),
        (
            "BlockInlining testShadowDoesntReadUnrelated",
            Value::new_class(&universe.nil_class()),
        ),
        (
            "BlockInlining testBlockNestedInIfTrue",
            Value::new_integer(2),
        ),
        (
            "BlockInlining testBlockNestedInIfFalse",
            Value::new_integer(42),
        ),
        (
            "BlockInlining testStackDisciplineTrue",
            Value::new_integer(1),
        ),
        (
            "BlockInlining testStackDisciplineFalse",
            Value::new_integer(2),
        ),
        (
            "BlockInlining testDeepNestedInlinedIfTrue",
            Value::new_integer(3),
        ),
        (
            "BlockInlining testDeepNestedInlinedIfFalse",
            Value::new_integer(42),
        ),
        (
            "BlockInlining testDeepNestedBlocksInInlinedIfTrue",
            Value::new_integer(5),
        ),
        (
            "BlockInlining testDeepNestedBlocksInInlinedIfFalse",
            Value::new_integer(43),
        ),
        (
            "BlockInlining testDeepDeepNestedTrue",
            Value::new_integer(9),
        ),
        (
            "BlockInlining testDeepDeepNestedFalse",
            Value::new_integer(43),
        ),
        (
            "BlockInlining testToDoNestDoNestIfTrue",
            Value::new_integer(2),
        ),
        (
            "NonLocalVars testWriteDifferentTypes",
            Value::new_double(3.75),
        ),
        ("ObjectCreation test", Value::new_integer(1000000)),
        ("Regressions testSymbolEquality", Value::new_integer(1)),
        (
            "Regressions testSymbolReferenceEquality",
            Value::new_integer(1),
        ),
        ("Regressions testUninitializedLocal", Value::new_integer(1)),
        (
            "Regressions testUninitializedLocalInBlock",
            Value::new_integer(1),
        ),
        ("BinaryOperation test", Value::new_integer(3 + 8)),
        ("NumberOfTests numberOfTests", Value::new_integer(65)),
    ];

    for (counter, (expr, expected)) in tests.iter().enumerate() {
        let mut interpreter = Interpreter::new();
        println!("testing: '{}'", expr);

        let line = format!(
            "BasicInterpreterTest{} = ( run = ( ^ ( {} ) ) )",
            counter, expr
        );

        let mut lexer = Lexer::new(line).skip_comments(true).skip_whitespace(true);
        let tokens: Vec<Token> = lexer.by_ref().collect();
        assert!(
            lexer.text().is_empty(),
            "could not fully tokenize test expression"
        );

        let class_def = som_parser::apply(lang::class_def(), tokens.as_slice()).unwrap();

        let object_class = universe.object_class();
        let class = compiler::compile_class(
            &mut heap,
            &mut universe.interner,
            &class_def,
            Some(&object_class),
        );
        assert!(class.is_some(), "could not compile test expression");
        let class = class.unwrap();

        let metaclass_class = universe.metaclass_class();
        class.borrow_mut().set_super_class(object_class.clone());
        class
            .borrow()
            .class()
            .borrow_mut()
            .set_super_class(object_class.borrow().class().clone());
        class
            .borrow()
            .class()
            .borrow_mut()
            .set_class(metaclass_class.clone());

        let method = class
            .borrow()
            .lookup_method(method_name)
            .expect("method not found ??");
        let kind = FrameKind::Method {
            method,
            holder: class.clone(),
            self_value: Value::new_class(&class),
        };
        interpreter.push_frame(&mut heap, kind);
        let output = interpreter.run(&mut heap, &mut universe).unwrap();
        assert_eq!(&output, expected, "unexpected test output value");
    }
}
