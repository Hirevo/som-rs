use som_core::bytecode::Bytecode;

use crate::block::Block;
use crate::class::Class;
use crate::compiler::Literal;
use crate::interner::Interned;
use crate::method::MethodEnv;
use crate::universe::Universe;

pub fn disassemble_method_body(universe: &Universe, class: &Class, env: &MethodEnv) {
    disassemble_body(universe, class, 1, &mut vec![env])
}

fn disassemble_body(
    universe: &Universe,
    class: &Class,
    level: usize,
    env: &mut Vec<&dyn FrameEnv>,
) {
    let padding = "  |".repeat(level);
    let current = env.last().copied().unwrap();
    for (cur_idx, bytecode) in current.get_body().into_iter().copied().enumerate() {
        let extra_spaces_nbr = if cur_idx >= 100 { 0 } else if (10..=99).contains(&cur_idx) { 1 } else { 2 };
        print!("{cur_idx} {0} {padding} {1}", " ".repeat(extra_spaces_nbr), bytecode.padded_name());
        // print!("{padding} {0}", bytecode.padded_name());

        match bytecode {
            Bytecode::Halt => {
                println!();
            }
            Bytecode::Dup => {
                println!();
            }
            Bytecode::PushLocal(up_idx, idx) | Bytecode::PopLocal(up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                let maybe_local = (env.iter().rev().nth(usize::from(up_idx)))
                    .and_then(|env| env.resolve_local(idx));
                let Some(local) = maybe_local else {
                    println!(" (invalid local)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(local));
            }
            Bytecode::PushField(idx) | Bytecode::PopField(idx) => {
                print!(" {idx}");
                let Some((name, _)) = class.locals.get_index(usize::from(idx)) else {
                    println!(" (invalid field)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(*name));
            }
            Bytecode::PushArgument(up_idx, idx) => {
                println!(" {up_idx}, {idx}");
            }
            Bytecode::PushBlock(idx) => {
                println!(" {idx}");
                let Some(Literal::Block(blk)) = current.resolve_literal(idx) else {
                    println!("{padding}  | (invalid block)");
                    continue;
                };
                env.push(blk.as_ref());
                disassemble_body(universe, class, level + 1, env);
                env.pop();
            }
            Bytecode::PushConstant(_)
            | Bytecode::PushConstant0
            | Bytecode::PushConstant1
            | Bytecode::PushConstant2 => {
                let idx = match bytecode {
                    Bytecode::PushConstant(c_idx) => c_idx,
                    Bytecode::PushConstant0 => 0,
                    Bytecode::PushConstant1 => 1,
                    Bytecode::PushConstant2 => 2,
                    _ => unreachable!(),
                };

                print!(" {idx}");
                let Some(literal) = current.resolve_literal(idx) else {
                    println!(" (invalid constant)");
                    continue;
                };
                match literal {
                    Literal::Symbol(symbol) => {
                        println!(" (Symbol(#{0}))", universe.lookup_symbol(*symbol));
                    }
                    _ => {
                        println!(" ({literal:?})");
                    }
                }
            }
            Bytecode::PushGlobal(idx) => {
                print!(" {idx}");
                let Some(Literal::Symbol(signature)) = current.resolve_literal(idx) else {
                    println!(" (invalid global)");
                    continue;
                };
                println!(" (`{0}`)", universe.lookup_symbol(*signature));
            }
            Bytecode::Pop => {
                println!();
            }
            Bytecode::PopArgument(up_idx, idx) => {
                print!(" {up_idx}, {idx}");
                // TODO: the following requires to change the parser and interpreter to preserve argument names.
                // let maybe_argument = (env.iter().rev().nth(usize::from(up_idx)))
                //     .and_then(|env| env.resolve_argument(idx));
                // let Some(argument) = maybe_argument else {
                //     println!(" (invalid argument)");
                //     continue;
                // };
                // println!(" (`{0}`)", universe.lookup_symbol(argument));
            }
            Bytecode::Send1(idx)
            | Bytecode::Send2(idx)
            | Bytecode::Send3(idx)
            | Bytecode::SendN(idx)
            | Bytecode::SuperSend1(idx)
            | Bytecode::SuperSend2(idx)
            | Bytecode::SuperSend3(idx)
            | Bytecode::SuperSendN(idx) => {
                print!(" {idx}");
                let Some(Literal::Symbol(signature)) = current.resolve_literal(idx) else {
                    println!(" (invalid signature)");
                    continue;
                };
                println!(" (#{0})", universe.lookup_symbol(*signature));
            },
            Bytecode::ReturnLocal => {
                println!();
            }
            Bytecode::ReturnNonLocal => {
                println!();
            },
            Bytecode::Jump(idx) |
            Bytecode::JumpOnFalsePop(idx) | Bytecode::JumpOnTruePop(idx) |
            Bytecode::JumpOnFalseTopNil(idx) | Bytecode::JumpOnTrueTopNil(idx) => {
                println!(" {} (jump to bytecode index {})", idx, cur_idx + idx);
            },
            Bytecode::JumpBackward(idx) => {
                println!(" {} (jump to bytecode index {})", idx, cur_idx - idx);
            }
            Bytecode::Push0 | Bytecode::Push1 | Bytecode::PushNil => {}
        }
    }
}

trait FrameEnv {
    fn get_body(&self) -> &[Bytecode];
    fn resolve_local(&self, idx: u8) -> Option<Interned>;
    fn resolve_literal(&self, idx: u8) -> Option<&Literal>;
    fn resolve_argument(&self, idx: u8) -> Option<Interned>;
}

impl FrameEnv for MethodEnv {
    fn get_body(&self) -> &[Bytecode] {
        &self.body
    }

    fn resolve_local(&self, idx: u8) -> Option<Interned> {
        self.locals.get(usize::from(idx)).copied()
    }

    fn resolve_literal(&self, idx: u8) -> Option<&Literal> {
        self.literals.get(usize::from(idx))
    }

    fn resolve_argument(&self, _idx: u8) -> Option<Interned> {
        todo!()
    }
}

impl FrameEnv for Block {
    fn get_body(&self) -> &[Bytecode] {
        &self.blk_info.body
    }

    fn resolve_local(&self, idx: u8) -> Option<Interned> {
        self.blk_info.locals.get(usize::from(idx)).copied()
    }

    fn resolve_literal(&self, idx: u8) -> Option<&Literal> {
        self.blk_info.literals.get(usize::from(idx))
    }

    fn resolve_argument(&self, _idx: u8) -> Option<Interned> {
        todo!()
    }
}
