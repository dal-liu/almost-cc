use std::fs;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;
use l1::*;

type L1Extra<'src> = extra::Err<Rich<'src, char>>;

pub fn parse_file(file_name: &str) -> Option<Program> {
    let file_name = file_name.to_owned();
    let input = fs::read_to_string(&file_name).unwrap_or_else(|e| panic!("{}", e));
    let (output, errors) = program().parse(&input).into_output_errors();

    errors.into_iter().for_each(|e| {
        Report::build(
            ReportKind::Error,
            (file_name.clone(), e.span().into_range()),
        )
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(e.to_string())
        .with_label(
            Label::new((file_name.clone(), e.span().into_range()))
                .with_message(e.reason().to_string())
                .with_color(Color::Red),
        )
        .finish()
        .eprint(sources([(file_name.clone(), input.clone())]))
        .unwrap();
    });

    output
}

fn separators<'src>() -> impl Parser<'src, &'src str, (), L1Extra<'src>> + Copy {
    one_of(" \t").repeated()
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), L1Extra<'src>> {
    just("//").ignore_then(none_of('\n').repeated()).padded()
}

fn write_register<'src>() -> impl Parser<'src, &'src str, Register, L1Extra<'src>> {
    choice((
        arg_register(),
        just("rax").to(Register::RAX),
        just("rbx").to(Register::RBX),
        just("rbp").to(Register::RBP),
        just("r10").to(Register::R10),
        just("r11").to(Register::R11),
        just("r12").to(Register::R12),
        just("r13").to(Register::R13),
        just("r14").to(Register::R14),
        just("r15").to(Register::R15),
    ))
    .padded_by(separators())
}

fn arg_register<'src>() -> impl Parser<'src, &'src str, Register, L1Extra<'src>> {
    choice((
        just("rdi").to(Register::RDI),
        just("rsi").to(Register::RSI),
        just("rdx").to(Register::RDX),
        rcx(),
        just("r8").to(Register::R8),
        just("r9").to(Register::R9),
    ))
    .padded_by(separators())
}

fn rcx<'src>() -> impl Parser<'src, &'src str, Register, L1Extra<'src>> {
    just("rcx").to(Register::RCX).padded_by(separators())
}

fn value<'src>() -> impl Parser<'src, &'src str, Value, L1Extra<'src>> {
    choice((
        register_or_number(),
        function_name().map(|callee| Value::Function(callee.to_owned())),
        label_name().map(|label| Value::Label(label.to_owned())),
    ))
    .padded_by(separators())
}

fn register_or_number<'src>() -> impl Parser<'src, &'src str, Value, L1Extra<'src>> {
    register()
        .map(Value::Register)
        .or(number().map(Value::Number))
        .padded_by(separators())
}

fn write_or_function<'src>() -> impl Parser<'src, &'src str, Value, L1Extra<'src>> {
    write_register()
        .map(Value::Register)
        .or(function_name().map(|callee| Value::Function(callee.to_owned())))
        .padded_by(separators())
}

fn register<'src>() -> impl Parser<'src, &'src str, Register, L1Extra<'src>> {
    write_register()
        .or(just("rsp").to(Register::RSP))
        .padded_by(separators())
}

fn arithmetic_op<'src>() -> impl Parser<'src, &'src str, ArithmeticOp, L1Extra<'src>> {
    choice((
        memory_arithmetic_op(),
        just("*=").to(ArithmeticOp::MulAssign),
        just("&=").to(ArithmeticOp::BitAndAssign),
    ))
    .padded_by(separators())
}

fn shift_op<'src>() -> impl Parser<'src, &'src str, ShiftOp, L1Extra<'src>> {
    choice((
        just("<<=").to(ShiftOp::ShlAssign),
        just(">>=").to(ShiftOp::ShrAssign),
    ))
    .padded_by(separators())
}

fn compare_op<'src>() -> impl Parser<'src, &'src str, CompareOp, L1Extra<'src>> {
    choice((
        just("<=").to(CompareOp::Le),
        just("<").to(CompareOp::Lt),
        just("=").to(CompareOp::Eq),
    ))
    .padded_by(separators())
}

fn multiplicative_of_8<'src>() -> impl Parser<'src, &'src str, i64, L1Extra<'src>> {
    number().filter(|n| n % 8 == 0).padded_by(separators())
}

fn number<'src>() -> impl Parser<'src, &'src str, i64, L1Extra<'src>> {
    just('+')
        .to(1)
        .or(just('-').to(-1))
        .or_not()
        .map(|opt| opt.unwrap_or(1))
        .then(text::int(10).from_str::<i128>().unwrapped())
        .map(|(sign, magnitude)| (sign * magnitude) as i64)
        .padded_by(separators())
}

fn function_name<'src>() -> impl Parser<'src, &'src str, &'src str, L1Extra<'src>> {
    just('@')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn label_name<'src>() -> impl Parser<'src, &'src str, &'src str, L1Extra<'src>> {
    just(':')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn rcx_or_number<'src>() -> impl Parser<'src, &'src str, Value, L1Extra<'src>> {
    rcx()
        .map(Value::Register)
        .or(number().map(Value::Number))
        .padded_by(separators())
}

fn memory_arithmetic_op<'src>() -> impl Parser<'src, &'src str, ArithmeticOp, L1Extra<'src>> {
    just("+=")
        .to(ArithmeticOp::AddAssign)
        .or(just("-=").to(ArithmeticOp::SubAssign))
        .padded_by(separators())
}

fn instruction<'src>() -> impl Parser<'src, &'src str, Instruction, L1Extra<'src>> {
    let arrow = just("<-").padded_by(separators());
    let mem = just("mem").padded_by(separators());
    let call_keyword = just("call").padded_by(separators());

    let assign = write_register()
        .then_ignore(arrow)
        .then(value())
        .map(|(dst, src)| Instruction::Assign { dst, src });

    let load = write_register()
        .then_ignore(arrow.then_ignore(mem))
        .then(register())
        .then(multiplicative_of_8())
        .map(|((dst, src), offset)| Instruction::Load { dst, src, offset });

    let store = mem
        .ignore_then(register())
        .then(multiplicative_of_8())
        .then_ignore(arrow)
        .then(value())
        .map(|((dst, offset), src)| Instruction::Store { dst, offset, src });

    let arithmetic = write_register()
        .then(arithmetic_op())
        .then(register_or_number())
        .map(|((dst, aop), src)| Instruction::Arithmetic { dst, aop, src });

    let shift = write_register()
        .then(shift_op())
        .then(rcx_or_number())
        .map(|((dst, sop), src)| Instruction::Shift { dst, sop, src });

    let store_arithmetic = mem
        .ignore_then(register())
        .then(multiplicative_of_8())
        .then(memory_arithmetic_op())
        .then(register_or_number())
        .map(|(((dst, offset), aop), src)| Instruction::StoreArithmetic {
            dst,
            offset,
            aop,
            src,
        });

    let load_arithmetic = write_register()
        .then(memory_arithmetic_op())
        .then_ignore(mem)
        .then(register())
        .then(multiplicative_of_8())
        .map(|(((dst, aop), src), offset)| Instruction::LoadArithmetic {
            dst,
            aop,
            src,
            offset,
        });

    let compare = write_register()
        .then_ignore(arrow)
        .then(register_or_number())
        .then(compare_op())
        .then(register_or_number())
        .map(|(((dst, lhs), cmp), rhs)| Instruction::Compare { dst, lhs, cmp, rhs });

    let cjump = just("cjump")
        .padded_by(separators())
        .ignore_then(register_or_number())
        .then(compare_op())
        .then(register_or_number())
        .then(label_name())
        .map(|(((lhs, cmp), rhs), label)| Instruction::CJump {
            lhs,
            cmp,
            rhs,
            label: label.to_owned(),
        });

    let label_inst = label_name().map(|label| Instruction::Label(label.to_owned()));

    let goto = just("goto")
        .padded_by(separators())
        .ignore_then(label_name())
        .map(|label| Instruction::Goto(label.to_owned()));

    let return_inst = just("return")
        .padded_by(separators())
        .to(Instruction::Return);

    let call_inst = call_keyword
        .ignore_then(write_or_function())
        .then(number())
        .map(|(callee, args)| Instruction::Call { callee, args });

    let print = call_keyword
        .then(just("print").padded_by(separators()))
        .then(just('1').padded_by(separators()))
        .to(Instruction::Print);

    let input = call_keyword
        .then(just("input").padded_by(separators()))
        .then(just('0').padded_by(separators()))
        .to(Instruction::Input);

    let allocate = call_keyword
        .then(just("allocate").padded_by(separators()))
        .then(just('2').padded_by(separators()))
        .to(Instruction::Allocate);

    let tuple_error = call_keyword
        .then(just("tuple-error").padded_by(separators()))
        .then(just('3').padded_by(separators()))
        .to(Instruction::TupleError);

    let tensor_error = call_keyword
        .ignore_then(just("tensor-error").padded_by(separators()))
        .ignore_then(
            text::int(10)
                .from_str::<u8>()
                .unwrapped()
                .filter(|&n| n == 1 || n == 3 || n == 4),
        )
        .map(Instruction::TensorError);

    let increment = write_register()
        .then_ignore(just("++").padded_by(separators()))
        .map(Instruction::Increment);

    let decrement = write_register()
        .then_ignore(just("--").padded_by(separators()))
        .map(Instruction::Decrement);

    let lea = write_register()
        .then_ignore(just('@').padded_by(separators()))
        .then(write_register())
        .then(write_register())
        .then(
            text::int(10)
                .from_str::<u8>()
                .unwrapped()
                .filter(|&n| n == 1 || n == 2 || n == 4 || n == 8),
        )
        .map(|(((dst, src), offset), scale)| Instruction::LEA {
            dst,
            src,
            offset,
            scale,
        });

    choice((
        compare,
        assign,
        load,
        store,
        arithmetic,
        shift,
        store_arithmetic,
        load_arithmetic,
        cjump,
        label_inst,
        goto,
        return_inst,
        call_inst,
        print,
        input,
        allocate,
        tuple_error,
        tensor_error,
        increment,
        decrement,
        lea,
    ))
}

fn function<'src>() -> impl Parser<'src, &'src str, Function, L1Extra<'src>> {
    just('(')
        .padded_by(comment().repeated())
        .padded()
        .ignore_then(function_name().padded_by(comment().repeated()).padded())
        .then(number().padded_by(comment().repeated()).padded())
        .then(number().padded_by(comment().repeated()).padded())
        .then(
            instruction()
                .padded_by(comment().repeated())
                .padded()
                .repeated()
                .at_least(1)
                .collect::<Vec<Instruction>>(),
        )
        .then_ignore(just(')').padded_by(comment().repeated()).padded())
        .map(|(((name, args), locals), instructions)| Function {
            name: name.to_owned(),
            args,
            locals,
            instructions,
        })
}

fn program<'src>() -> impl Parser<'src, &'src str, Program, L1Extra<'src>> {
    just('(')
        .padded_by(comment().repeated())
        .padded()
        .ignore_then(function_name().padded_by(comment().repeated()).padded())
        .then(function().repeated().at_least(1).collect::<Vec<Function>>())
        .then_ignore(
            just(')')
                .padded_by(comment().repeated())
                .padded()
                .then(any().repeated()),
        )
        .map(|(entry_point, functions)| Program {
            entry_point: entry_point.to_owned(),
            functions,
        })
}
