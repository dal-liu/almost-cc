use std::fs::File;
use std::io::{self, BufWriter, Write};

use l1::*;

struct CodeGenerator {
    stream: BufWriter<File>,
}

impl CodeGenerator {
    fn new() -> io::Result<Self> {
        let file = File::create("prog.S")?;
        Ok(Self {
            stream: BufWriter::new(file),
        })
    }

    fn emit_program(&mut self, prog: &Program) -> io::Result<()> {
        writeln!(
            self.stream,
            "\t.text\n\
             \t.globl go\n\
             go:\n\
             \tpushq %rbx\n\
             \tpushq %rbp\n\
             \tpushq %r12\n\
             \tpushq %r13\n\
             \tpushq %r14\n\
             \tpushq %r15\n\
             \tcall _{}\n\
             \tpopq %r15\n\
             \tpopq %r14\n\
             \tpopq %r13\n\
             \tpopq %r12\n\
             \tpopq %rbp\n\
             \tpopq %rbx\n\
             \tretq",
            prog.entry_point
        )?;

        for func in &prog.functions {
            self.emit_function(func)?;
        }

        Ok(())
    }

    fn emit_function(&mut self, func: &Function) -> io::Result<()> {
        writeln!(self.stream, "_{}:", func.name)?;

        if func.locals > 0 {
            writeln!(self.stream, "\tsubq ${}, %rsp", func.locals * 8)?;
        }

        for inst in &func.instructions {
            self.emit_instruction(inst, func.args, func.locals)?;
        }

        Ok(())
    }

    fn emit_instruction(&mut self, inst: &Instruction, args: i64, locals: i64) -> io::Result<()> {
        use Instruction::*;

        match inst {
            Assign { dst, src } => {
                writeln!(self.stream, "\tmovq {}, %{}", self.format_value(src), dst)
            }
            Load { dst, src, offset } => {
                writeln!(self.stream, "\tmovq {}(%{}), %{}", offset, src, dst)
            }
            Store { dst, offset, src } => {
                writeln!(
                    self.stream,
                    "\tmovq {}, {}(%{})",
                    self.format_value(src),
                    offset,
                    dst
                )
            }
            Arithmetic { dst, aop, src } => {
                let arith = match aop {
                    ArithmeticOp::AddAssign => "addq",
                    ArithmeticOp::SubAssign => "subq",
                    ArithmeticOp::MulAssign => "imulq",
                    ArithmeticOp::BitAndAssign => "andq",
                };
                writeln!(
                    self.stream,
                    "\t{} {}, %{}",
                    arith,
                    self.format_value(src),
                    dst
                )
            }
            Shift { dst, sop, src } => {
                let shift = match sop {
                    ShiftOp::ShlAssign => "salq",
                    ShiftOp::ShrAssign => "sarq",
                };
                writeln!(
                    self.stream,
                    "\t{} {}, %{}",
                    shift,
                    self.format_value_8_bit(src),
                    dst
                )
            }
            StoreArithmetic {
                dst,
                offset,
                aop,
                src,
            } => {
                let arith = match aop {
                    ArithmeticOp::AddAssign => "addq",
                    ArithmeticOp::SubAssign => "subq",
                    _ => panic!("store arithmetic invalid op"),
                };
                writeln!(
                    self.stream,
                    "\t{} {}, {}(%{})",
                    arith,
                    self.format_value(src),
                    offset,
                    dst
                )
            }
            LoadArithmetic {
                dst,
                aop,
                src,
                offset,
            } => {
                let arith = match aop {
                    ArithmeticOp::AddAssign => "addq",
                    ArithmeticOp::SubAssign => "subq",
                    _ => panic!("load arithmetic invalid op"),
                };
                writeln!(self.stream, "\t{} {}(%{}), %{}", arith, offset, src, dst)
            }
            Compare { dst, lhs, cmp, rhs } => {
                if let (Value::Number(a), Value::Number(b)) = (lhs, rhs) {
                    let res = match cmp {
                        CompareOp::Lt => a < b,
                        CompareOp::Le => a <= b,
                        CompareOp::Eq => a == b,
                    };
                    writeln!(self.stream, "\tmovq ${}, %{}", res as u8, dst)
                } else if let Value::Number(n) = lhs {
                    writeln!(self.stream, "\tcmpq ${}, {}", n, self.format_value(rhs))?;
                    let cmp = match cmp {
                        CompareOp::Lt => "setg",
                        CompareOp::Le => "setge",
                        CompareOp::Eq => "sete",
                    };
                    let dst_8_bit = self.format_register_8_bit(dst);
                    writeln!(self.stream, "\t{} {}", cmp, dst_8_bit)?;
                    writeln!(self.stream, "\tmovzbq {}, %{}", dst_8_bit, dst)
                } else {
                    writeln!(
                        self.stream,
                        "\tcmpq {}, {}",
                        self.format_value(rhs),
                        self.format_value(lhs)
                    )?;
                    let cmp = match cmp {
                        CompareOp::Lt => "setl",
                        CompareOp::Le => "setle",
                        CompareOp::Eq => "sete",
                    };
                    let dst_8_bit = self.format_register_8_bit(dst);
                    writeln!(self.stream, "\t{} {}", cmp, dst_8_bit)?;
                    writeln!(self.stream, "\tmovzbq {}, %{}", dst_8_bit, dst)
                }
            }
            CJump {
                lhs,
                cmp,
                rhs,
                label,
            } => {
                if let (Value::Number(a), Value::Number(b)) = (lhs, rhs) {
                    let res = match cmp {
                        CompareOp::Lt => a < b,
                        CompareOp::Le => a <= b,
                        CompareOp::Eq => a == b,
                    };
                    if res {
                        writeln!(self.stream, "\tjmp _{}", label)
                    } else {
                        Ok(())
                    }
                } else if let Value::Number(n) = lhs {
                    writeln!(self.stream, "\tcmpq ${}, {}", n, self.format_value(rhs))?;
                    let jmp = match cmp {
                        CompareOp::Lt => "jg",
                        CompareOp::Le => "jge",
                        CompareOp::Eq => "je",
                    };
                    writeln!(self.stream, "\t{} _{}", jmp, label)
                } else {
                    writeln!(
                        self.stream,
                        "\tcmpq {}, {}",
                        self.format_value(rhs),
                        self.format_value(lhs)
                    )?;
                    let jmp = match cmp {
                        CompareOp::Lt => "jl",
                        CompareOp::Le => "jle",
                        CompareOp::Eq => "je",
                    };
                    writeln!(self.stream, "\t{} _{}", jmp, label)
                }
            }
            Label(label) => writeln!(self.stream, "_{}:", label),
            Goto(label) => writeln!(self.stream, "\tjmp _{}", label),
            Return => {
                let stack_size = (locals + (args - 6).max(0)) * 8;
                if stack_size > 0 {
                    writeln!(self.stream, "\taddq ${}, %rsp", stack_size)?;
                }
                writeln!(self.stream, "\tretq")
            }
            Call { callee, args } => {
                writeln!(self.stream, "\tsubq ${}, %rsp", (args - 6).max(0) * 8 + 8)?;
                let name = match callee {
                    Value::Register(reg) => format!("*%{}", reg),
                    Value::Function(label) => format!("_{}", label),
                    _ => panic!("call invalid callee"),
                };
                writeln!(self.stream, "\tjmp {}", name)
            }
            Print => writeln!(self.stream, "\tcall print"),
            Allocate => writeln!(self.stream, "\tcall allocate"),
            Input => writeln!(self.stream, "\tcall input"),
            TupleError => writeln!(self.stream, "\tcall tuple_error"),
            TensorError(args) => {
                let callee = match args {
                    1 => "array_tensor_error_null",
                    3 => "array_error",
                    4 => "tensor_error",
                    _ => panic!("tensor error invalid args"),
                };
                writeln!(self.stream, "\tcall {}", callee)
            }
            Increment(reg) => writeln!(self.stream, "\tinc %{}", reg),
            Decrement(reg) => writeln!(self.stream, "\tdec %{}", reg),
            LEA {
                dst,
                src,
                offset,
                scale,
            } => {
                writeln!(
                    self.stream,
                    "\tlea (%{}, %{}, {}), %{}",
                    src, offset, scale, dst
                )
            }
        }
    }

    fn format_value(&self, val: &Value) -> String {
        match val {
            Value::Register(r) => format!("%{}", r),
            Value::Number(n) => format!("${}", n),
            Value::Label(s) => format!("$_{}", s),
            Value::Function(s) => format!("$_{}", s),
        }
    }

    fn format_value_8_bit(&self, val: &Value) -> String {
        match val {
            Value::Register(r) => self.format_register_8_bit(r).into(),
            Value::Number(n) => format!("${}", n),
            Value::Label(s) => format!("$_{}", s),
            Value::Function(s) => format!("$_{}", s),
        }
    }

    fn format_register_8_bit(&self, reg: &Register) -> &'static str {
        use Register::*;

        match reg {
            RAX => "%al",
            RBX => "%bl",
            RBP => "%bpl",
            R10 => "%r10b",
            R11 => "%r11b",
            R12 => "%r12b",
            R13 => "%r13b",
            R14 => "%r14b",
            R15 => "%r15b",
            RDI => "%dil",
            RSI => "%sil",
            RDX => "%dl",
            R8 => "%r8b",
            R9 => "%r9b",
            RCX => "%cl",
            RSP => panic!("rsp cannot be 8 bit"),
        }
    }

    fn finish(mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

pub fn generate_code(prog: &Program) -> io::Result<()> {
    let mut code_generator = CodeGenerator::new()?;
    code_generator.emit_program(prog)?;
    code_generator.finish()
}
