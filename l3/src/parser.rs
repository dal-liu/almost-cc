use std::fs;
use std::mem;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;
use l3::*;
use utils::Interner;

type MyExtra<'src> = extra::Full<Rich<'src, char>, extra::SimpleState<Interner<String>>, ()>;

pub fn parse_file(file_name: &str) -> Option<Program> {
    let file_name = file_name.to_owned();
    let input = fs::read_to_string(&file_name).unwrap_or_else(|e| panic!("{}", e));

    let (output, errors) = program()
        .parse_with_state(&input, &mut extra::SimpleState(Interner::new()))
        .into_output_errors();

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

fn separators<'src>() -> impl Parser<'src, &'src str, (), MyExtra<'src>> + Copy {
    one_of(" \t").repeated()
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), MyExtra<'src>> {
    just("//").ignore_then(none_of('\n').repeated()).padded()
}

fn callee<'src>() -> impl Parser<'src, &'src str, Callee, MyExtra<'src>> {
    choice((
        variable_or_function().map(Callee::Value),
        just("print").to(Callee::Print),
        just("allocate").to(Callee::Allocate),
        just("input").to(Callee::Input),
        just("tuple-error").to(Callee::TupleError),
        just("tensor-error").to(Callee::TensorError),
    ))
    .padded_by(separators())
}

fn variables<'src>() -> impl Parser<'src, &'src str, Vec<SymbolId>, MyExtra<'src>> {
    variable_name()
        .map_with(|name, e| SymbolId(e.state().intern(name.to_owned())))
        .separated_by(just(','))
        .collect::<Vec<SymbolId>>()
        .padded_by(separators())
}

fn arguments<'src>() -> impl Parser<'src, &'src str, Vec<Value>, MyExtra<'src>> {
    variable_or_number()
        .separated_by(just(','))
        .collect::<Vec<Value>>()
        .padded_by(separators())
}

fn value<'src>() -> impl Parser<'src, &'src str, Value, MyExtra<'src>> {
    choice((
        variable_or_number(),
        label_name()
            .map_with(|label, e| Value::Label(SymbolId(e.state().intern(label.to_owned())))),
        function_name()
            .map_with(|callee, e| Value::Function(SymbolId(e.state().intern(callee.to_owned())))),
    ))
    .padded_by(separators())
}

fn variable_or_number<'src>() -> impl Parser<'src, &'src str, Value, MyExtra<'src>> {
    variable_name()
        .map_with(|var, e| Value::Variable(SymbolId(e.state().intern(var.to_owned()))))
        .or(number().map(Value::Number))
        .padded_by(separators())
}

fn variable_or_function<'src>() -> impl Parser<'src, &'src str, Value, MyExtra<'src>> {
    variable_name()
        .map_with(|var, e| Value::Variable(SymbolId(e.state().intern(var.to_owned()))))
        .or(function_name()
            .map_with(|callee, e| Value::Function(SymbolId(e.state().intern(callee.to_owned())))))
        .padded_by(separators())
}

fn binary_op<'src>() -> impl Parser<'src, &'src str, BinaryOp, MyExtra<'src>> {
    choice((
        just('+').to(BinaryOp::Add),
        just('-').to(BinaryOp::Sub),
        just('*').to(BinaryOp::Mul),
        just('&').to(BinaryOp::BitAnd),
        just("<<").to(BinaryOp::Shl),
        just(">>").to(BinaryOp::Shr),
    ))
    .padded_by(separators())
}

fn compare_op<'src>() -> impl Parser<'src, &'src str, CompareOp, MyExtra<'src>> {
    choice((
        just("<=").to(CompareOp::Le),
        just(">=").to(CompareOp::Ge),
        just('<').to(CompareOp::Lt),
        just('=').to(CompareOp::Eq),
        just('>').to(CompareOp::Gt),
    ))
    .padded_by(separators())
}

fn number<'src>() -> impl Parser<'src, &'src str, i64, MyExtra<'src>> {
    just('+')
        .to(1)
        .or(just('-').to(-1))
        .or_not()
        .map(|opt| opt.unwrap_or(1))
        .then(text::int(10).from_str::<i128>().unwrapped())
        .map(|(sign, magnitude)| (sign * magnitude) as i64)
        .padded_by(separators())
}

fn function_name<'src>() -> impl Parser<'src, &'src str, &'src str, MyExtra<'src>> {
    just('@')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn label_name<'src>() -> impl Parser<'src, &'src str, &'src str, MyExtra<'src>> {
    just(':')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn variable_name<'src>() -> impl Parser<'src, &'src str, &'src str, MyExtra<'src>> {
    just('%')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn instruction<'src>() -> impl Parser<'src, &'src str, Instruction, MyExtra<'src>> {
    let arrow = just("<-").padded_by(separators());
    let call_keyword = just("call").padded_by(separators());
    let return_keyword = just("return").padded_by(separators());
    let br_keyword = just("br").padded_by(separators());

    let assign = variable_name()
        .then_ignore(arrow)
        .then(value())
        .map_with(|(dst, src), e| Instruction::Assign {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            src,
        });

    let binary = variable_name()
        .then_ignore(arrow)
        .then(variable_or_number())
        .then(binary_op())
        .then(variable_or_number())
        .map_with(|(((dst, lhs), op), rhs), e| Instruction::Binary {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            lhs,
            op,
            rhs,
        });

    let compare = variable_name()
        .then_ignore(arrow)
        .then(variable_or_number())
        .then(compare_op())
        .then(variable_or_number())
        .map_with(|(((dst, lhs), cmp), rhs), e| Instruction::Compare {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            lhs,
            cmp,
            rhs,
        });

    let load = variable_name()
        .then_ignore(arrow)
        .then_ignore(just("load").padded_by(separators()))
        .then(variable_name())
        .map_with(|(dst, src), e| Instruction::Load {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            src: SymbolId(e.state().intern(src.to_owned())),
        });

    let store = just("store")
        .padded_by(separators())
        .ignore_then(variable_name())
        .then_ignore(arrow)
        .then(value())
        .map_with(|(dst, src), e| Instruction::Store {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            src,
        });

    let return_inst = return_keyword.to(Instruction::Return);

    let return_value = return_keyword
        .ignore_then(variable_or_number())
        .map(Instruction::ReturnValue);

    let label_inst = label_name()
        .map_with(|label, e| Instruction::Label(SymbolId(e.state().intern(label.to_owned()))));

    let branch = br_keyword
        .ignore_then(label_name().map_with(|label, e| {
            Instruction::Branch(SymbolId(e.state().intern(label.to_owned())))
        }));

    let branch_cond = br_keyword
        .ignore_then(variable_or_number())
        .then(label_name())
        .map_with(|(cond, label), e| Instruction::BranchCond {
            cond,
            label: SymbolId(e.state().intern(label.to_owned())),
        });

    let call_inst = call_keyword
        .ignore_then(callee())
        .then_ignore(just('(').padded_by(separators()))
        .then(arguments())
        .then_ignore(just(')').padded_by(separators()))
        .map(|(callee, args)| Instruction::Call { callee, args });

    let call_result = variable_name()
        .then_ignore(arrow.then(call_keyword))
        .then(callee())
        .then_ignore(just('(').padded_by(separators()))
        .then(arguments())
        .then_ignore(just(')').padded_by(separators()))
        .map_with(|((res, callee), args), e| Instruction::CallResult {
            dst: SymbolId(e.state().intern(res.to_owned())),
            callee,
            args,
        });

    choice((
        binary,
        compare,
        assign,
        load,
        store,
        return_value,
        return_inst,
        label_inst,
        branch,
        branch_cond,
        call_inst,
        call_result,
    ))
}

fn function<'src>() -> impl Parser<'src, &'src str, Function, MyExtra<'src>> {
    just("define")
        .padded_by(separators())
        .ignore_then(function_name())
        .then_ignore(just('(').padded_by(separators()))
        .then(variables())
        .then_ignore(just(')').padded_by(separators()))
        .then_ignore(just('{').padded_by(comment().repeated()).padded())
        .then(
            instruction()
                .padded_by(comment().repeated())
                .padded()
                .repeated()
                .at_least(1)
                .collect::<Vec<Instruction>>(),
        )
        .then_ignore(just('}').padded_by(comment().repeated()).padded())
        .map_with(|((name, params), instructions), e| {
            Function::new(
                SymbolId(e.state().intern(name.to_owned())),
                params,
                instructions,
            )
        })
}

fn program<'src>() -> impl Parser<'src, &'src str, Program, MyExtra<'src>> {
    function()
        .padded_by(comment().repeated())
        .padded()
        .repeated()
        .at_least(1)
        .collect::<Vec<Function>>()
        .map_with(|functions, e| Program {
            functions,
            interner: mem::take(e.state()),
        })
}
