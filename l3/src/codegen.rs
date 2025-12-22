use std::cmp;
use std::fs::File;
use std::io::{self, BufWriter, Write};

use l2;
use l3::*;
use utils::{DisplayResolved, Interner};

use crate::analysis::{DefUseChain, compute_liveness, compute_reaching_def};
use crate::isel::contexts::create_contexts;
use crate::isel::forest::{NodeId, NodeKind, SelectionForest, generate_forest};
use crate::isel::tiling::{Cover, cover_forest, isel_tiles};
use crate::translation::{translate_symbol_id, translate_value};

const LABEL_OFFSET: usize = 1;

const ARG_REGISTERS: &[l2::Register] = &[
    l2::Register::RDI,
    l2::Register::RSI,
    l2::Register::RDX,
    l2::Register::RCX,
    l2::Register::R8,
    l2::Register::R9,
];

struct CodeGenerator {
    stream: BufWriter<File>,
}

impl CodeGenerator {
    fn new() -> io::Result<Self> {
        let file = File::create("prog.L2")?;
        Ok(Self {
            stream: BufWriter::new(file),
        })
    }

    fn emit_program(
        &mut self,
        prog: &mut Program,
        prefix: &str,
        suffix: &mut u32,
    ) -> io::Result<()> {
        writeln!(self.stream, "(@main")?;

        for func in &prog.functions {
            self.emit_function(func, &mut prog.interner, prefix, suffix)?;
        }

        writeln!(self.stream, ")")
    }

    fn emit_function(
        &mut self,
        func: &Function,
        interner: &mut Interner<String>,
        prefix: &str,
        suffix: &mut u32,
    ) -> io::Result<()> {
        let num_params = func.params.len();
        let num_arg_registers = ARG_REGISTERS.len();

        writeln!(
            self.stream,
            "  (@{} {}",
            interner.resolve(func.name.0),
            num_params
        )?;

        for i in 0..cmp::min(num_params, num_arg_registers) {
            writeln!(
                self.stream,
                "    {}",
                l2::Instruction::Assign {
                    dst: l2::Value::Variable(translate_symbol_id(func.params[i])),
                    src: l2::Value::Register(ARG_REGISTERS[i])
                }
                .resolved(interner)
            )?;
        }

        for i in num_arg_registers..num_params {
            writeln!(
                self.stream,
                "    {}",
                l2::Instruction::StackArg {
                    dst: l2::Value::Variable(translate_symbol_id(func.params[i])),
                    offset: (i - num_arg_registers) as i64 * 8
                }
                .resolved(interner)
            )?;
        }

        let liveness = compute_liveness(func);
        let reaching_def = compute_reaching_def(func);
        let def_use = DefUseChain::new(func, &reaching_def);
        let tiles = isel_tiles();

        fn dfs(
            forest: &SelectionForest,
            id: NodeId,
            stream: &mut BufWriter<File>,
            cover: &Cover,
            interner: &Interner<String>,
        ) -> io::Result<()> {
            let node = forest.node(id);

            for &child in &node.children {
                if matches!(&node.kind, NodeKind::Op(_)) {
                    dfs(forest, child, stream, cover, interner)?;
                }
            }

            if let Some(tile) = cover.map.get(&id) {
                for l2_inst in (tile.emit)(forest, id) {
                    writeln!(stream, "    {}", l2_inst.resolved(interner))?;
                }
            }

            Ok(())
        }

        for ctx in create_contexts(func) {
            let forest = generate_forest(func, &liveness, &def_use, &ctx);

            for cover in cover_forest(&forest, &tiles) {
                dfs(&forest, cover.root, &mut self.stream, &cover, interner)?;
            }

            if let Some(term) = ctx.terminator {
                self.emit_instruction(term, interner, prefix, suffix)?;
            }
        }

        writeln!(self.stream, "  )")
    }

    fn emit_instruction(
        &mut self,
        inst: &Instruction,
        interner: &mut Interner<String>,
        prefix: &str,
        suffix: &mut u32,
    ) -> io::Result<()> {
        let mut emit_call = |callee: &Callee, args: &[Value]| {
            let num_args = args.len();
            let num_arg_registers = ARG_REGISTERS.len();

            for i in 0..cmp::min(num_args, num_arg_registers) {
                writeln!(
                    self.stream,
                    "    {}",
                    l2::Instruction::Assign {
                        dst: l2::Value::Register(ARG_REGISTERS[i]),
                        src: translate_value(&args[i])
                    }
                    .resolved(interner)
                )?;
            }

            for i in num_arg_registers..num_args {
                writeln!(
                    self.stream,
                    "    {}",
                    l2::Instruction::Store {
                        dst: l2::Value::Register(l2::Register::RSP),
                        offset: (num_args - i + LABEL_OFFSET) as i64 * -8,
                        src: translate_value(&args[i])
                    }
                    .resolved(interner)
                )?;
            }

            let l2_inst = match callee {
                Callee::Value(val) => l2::Instruction::Call {
                    callee: translate_value(val),
                    args: num_args as i64,
                },
                Callee::Print => l2::Instruction::Print,
                Callee::Allocate => l2::Instruction::Allocate,
                Callee::Input => l2::Instruction::Input,
                Callee::TupleError => l2::Instruction::TupleError,
                Callee::TensorError => l2::Instruction::TensorError(num_args as u8),
            };

            if !callee.is_libcall() {
                let l2_id = l2::SymbolId(interner.intern(format!("{}{}", prefix, suffix)));
                *suffix += 1;

                writeln!(
                    self.stream,
                    "    {}",
                    l2::Instruction::Store {
                        dst: l2::Value::Register(l2::Register::RSP),
                        offset: -8,
                        src: l2::Value::Label(l2_id)
                    }
                    .resolved(interner)
                )?;

                writeln!(self.stream, "    {}", l2_inst.resolved(interner))?;

                writeln!(
                    self.stream,
                    "    {}",
                    l2::Instruction::Label(l2_id).resolved(interner)
                )?;
            } else {
                writeln!(self.stream, "    {}", l2_inst.resolved(interner))?;
            }

            Ok(())
        };

        match inst {
            Instruction::Label(label) => {
                writeln!(self.stream, "    :{}", interner.resolve(label.0))
            }

            Instruction::Call { callee, args } => emit_call(callee, args),

            Instruction::CallResult { dst, callee, args } => {
                emit_call(callee, args)?;
                writeln!(
                    self.stream,
                    "    {}",
                    l2::Instruction::Assign {
                        dst: l2::Value::Variable(translate_symbol_id(*dst)),
                        src: l2::Value::Register(l2::Register::RAX)
                    }
                    .resolved(interner)
                )
            }

            _ => unreachable!("instruction should be emitted by tile"),
        }
    }

    fn finish(mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

pub fn generate_code(prog: &mut Program, prefix: &str, suffix: &mut u32) -> io::Result<()> {
    let mut code_generator = CodeGenerator::new()?;
    code_generator.emit_program(prog, prefix, suffix)?;
    code_generator.finish()
}
