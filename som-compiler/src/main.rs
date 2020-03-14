use som_lexer::Lexer;

static CODE: &str = r#"
Fibonacci = (
    fib: n = (
        ^ n <= 1
            ifTrue:  1
            ifFalse: [ self fib: (n - 1) + (self fib: (n - 2)) ]
    )
)
"#;

fn main() {
    let mut lexer = Lexer::new(CODE);

    for symbol in &mut lexer {
        println!("{:?}", symbol);
    }

    let remaining = lexer.text();
    if !remaining.is_empty() {
        println!();
        println!("remaining: {}", remaining);
    }
}
