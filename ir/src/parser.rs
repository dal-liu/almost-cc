use std::fs;
use std::mem;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;
use ir::*;
use utils::Interner;

type IRExtra<'src> = extra::Full<Rich<'src, char>, extra::SimpleState<Interner<String>>, ()>;

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

fn separators<'src>() -> impl Parser<'src, &'src str, (), IRExtra<'src>> + Copy {
    one_of(" \t").repeated()
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), IRExtra<'src>> {
    just("//").ignore_then(none_of('\n').repeated()).padded()
}

fn type_or_void<'src>() -> impl Parser<'src, &'src str, Type, IRExtra<'src>> {
    choice((type_(), just("void").to(Type::Void))).padded_by(separators())
}

fn type_<'src>() -> impl Parser<'src, &'src str, Type, IRExtra<'src>> {
    choice((
        just("int64")
            .ignore_then(just("[]").repeated().at_least(1).count())
            .map(|ndims| Type::Array(ndims)),
        just("int64").to(Type::Int64),
        just("tuple").to(Type::Tuple),
        just("code").to(Type::Code),
    ))
    .padded_by(separators())
}

fn callee<'src>() -> impl Parser<'src, &'src str, Callee, IRExtra<'src>> {
    choice((
        variable_or_function().map(Callee::Value),
        just("print").to(Callee::Print),
        just("input").to(Callee::Input),
        just("tuple-error").to(Callee::TupleError),
        just("tensor-error").to(Callee::TensorError),
    ))
    .padded_by(separators())
}

fn parameters<'src>() -> impl Parser<'src, &'src str, Vec<Parameter>, IRExtra<'src>> {
    type_()
        .then(variable_name())
        .map_with(|(ty, var), e| Parameter {
            ty,
            var: SymbolId(e.state().intern(var.to_owned())),
        })
        .separated_by(just(','))
        .collect::<Vec<Parameter>>()
        .padded_by(separators())
}

fn arguments<'src>() -> impl Parser<'src, &'src str, Vec<Value>, IRExtra<'src>> {
    variable_or_number()
        .separated_by(just(','))
        .at_least(1)
        .collect::<Vec<Value>>()
        .padded_by(separators())
}

fn value<'src>() -> impl Parser<'src, &'src str, Value, IRExtra<'src>> {
    choice((
        variable_or_number(),
        function_name()
            .map_with(|callee, e| Value::Function(SymbolId(e.state().intern(callee.to_owned())))),
    ))
    .padded_by(separators())
}

fn variable_or_number<'src>() -> impl Parser<'src, &'src str, Value, IRExtra<'src>> {
    variable_name()
        .map_with(|var, e| Value::Variable(SymbolId(e.state().intern(var.to_owned()))))
        .or(number().map(Value::Number))
        .padded_by(separators())
}

fn variable_or_function<'src>() -> impl Parser<'src, &'src str, Value, IRExtra<'src>> {
    variable_name()
        .map_with(|var, e| Value::Variable(SymbolId(e.state().intern(var.to_owned()))))
        .or(function_name()
            .map_with(|callee, e| Value::Function(SymbolId(e.state().intern(callee.to_owned())))))
        .padded_by(separators())
}

fn number<'src>() -> impl Parser<'src, &'src str, i64, IRExtra<'src>> {
    just('+')
        .to(1)
        .or(just('-').to(-1))
        .or_not()
        .map(|opt| opt.unwrap_or(1))
        .then(text::int(10).from_str::<i128>().unwrapped())
        .map(|(sign, magnitude)| (sign * magnitude) as i64)
        .padded_by(separators())
}

fn binary_op<'src>() -> impl Parser<'src, &'src str, BinaryOp, IRExtra<'src>> {
    choice((
        just('+').to(BinaryOp::Add),
        just('-').to(BinaryOp::Sub),
        just('*').to(BinaryOp::Mul),
        just('&').to(BinaryOp::BitAnd),
        just("<<").to(BinaryOp::Shl),
        just(">>").to(BinaryOp::Shr),
        just("<=").to(BinaryOp::Le),
        just(">=").to(BinaryOp::Ge),
        just('<').to(BinaryOp::Lt),
        just('=').to(BinaryOp::Eq),
        just('>').to(BinaryOp::Gt),
    ))
    .padded_by(separators())
}

fn function_name<'src>() -> impl Parser<'src, &'src str, &'src str, IRExtra<'src>> {
    just('@')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn label_name<'src>() -> impl Parser<'src, &'src str, &'src str, IRExtra<'src>> {
    just(':')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn variable_name<'src>() -> impl Parser<'src, &'src str, &'src str, IRExtra<'src>> {
    just('%')
        .ignore_then(text::ascii::ident())
        .padded_by(separators())
}

fn instruction<'src>() -> impl Parser<'src, &'src str, Instruction, IRExtra<'src>> {
    let arrow = just("<-").padded_by(separators());
    let call_keyword = just("call").padded_by(separators());
    let length_keyword = just("length").padded_by(separators());
    let new_keyword = just("new").padded_by(separators());

    let define = type_()
        .then(variable_name())
        .map_with(|(ty, var), e| Instruction::Define {
            ty,
            var: SymbolId(e.state().intern(var.to_owned())),
        });

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

    let extract = variable_name()
        .then_ignore(arrow)
        .then(variable_name())
        .then(
            just('[')
                .padded_by(separators())
                .ignore_then(variable_or_number())
                .then_ignore(just(']').padded_by(separators()))
                .repeated()
                .at_least(1)
                .collect::<Vec<Value>>(),
        )
        .map_with(|((dst, src), idxs), e| Instruction::Extract {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            src: Value::Variable(SymbolId(e.state().intern(src.to_owned()))),
            idxs,
        });

    let insert = variable_name()
        .then(
            just('[')
                .padded_by(separators())
                .ignore_then(variable_or_number())
                .then_ignore(just(']').padded_by(separators()))
                .repeated()
                .at_least(1)
                .collect::<Vec<Value>>(),
        )
        .then_ignore(arrow)
        .then(value())
        .map_with(|((dst, idxs), src), e| Instruction::Insert {
            dst: Value::Variable(SymbolId(e.state().intern(dst.to_owned()))),
            idxs,
            src,
        });

    let array_length = variable_name()
        .then_ignore(arrow)
        .then_ignore(length_keyword)
        .then(variable_name())
        .then(variable_or_number())
        .map_with(|((dst, src), dim), e| Instruction::ArrayLength {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            src: Value::Variable(SymbolId(e.state().intern(src.to_owned()))),
            dim,
        });

    let tuple_length = variable_name()
        .then_ignore(arrow)
        .then_ignore(length_keyword)
        .then(variable_name())
        .map_with(|(dst, src), e| Instruction::TupleLength {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            src: Value::Variable(SymbolId(e.state().intern(src.to_owned()))),
        });

    let call_inst = call_keyword
        .ignore_then(callee())
        .then_ignore(just('(').padded_by(separators()))
        .then(arguments().or_not())
        .then_ignore(just(')').padded_by(separators()))
        .map(|(callee, args)| Instruction::Call {
            callee,
            args: args.unwrap_or_default(),
        });

    let call_result = variable_name()
        .then_ignore(arrow.then(call_keyword))
        .then(callee())
        .then_ignore(just('(').padded_by(separators()))
        .then(arguments().or_not())
        .then_ignore(just(')').padded_by(separators()))
        .map_with(|((res, callee), args), e| Instruction::CallResult {
            dst: SymbolId(e.state().intern(res.to_owned())),
            callee,
            args: args.unwrap_or_default(),
        });

    let new_array = variable_name()
        .then_ignore(arrow)
        .then_ignore(new_keyword)
        .then_ignore(just("Array").padded_by(separators()))
        .then_ignore(just('(').padded_by(separators()))
        .then(arguments())
        .then_ignore(just(')').padded_by(separators()))
        .map_with(|(dst, dims), e| Instruction::NewArray {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            dims,
        });

    let new_tuple = variable_name()
        .then_ignore(arrow)
        .then_ignore(new_keyword)
        .then_ignore(just("Tuple").padded_by(separators()))
        .then_ignore(just('(').padded_by(separators()))
        .then(variable_or_number())
        .then_ignore(just(')').padded_by(separators()))
        .map_with(|(dst, len), e| Instruction::NewTuple {
            dst: SymbolId(e.state().intern(dst.to_owned())),
            len,
        });

    choice((
        define,
        binary,
        extract,
        assign,
        insert,
        array_length,
        tuple_length,
        call_inst,
        call_result,
        new_array,
        new_tuple,
    ))
    .padded_by(comment().repeated())
    .padded()
}

fn terminator<'src>() -> impl Parser<'src, &'src str, Terminator, IRExtra<'src>> {
    let br_keyword = just("br").padded_by(separators());
    let return_keyword = just("return").padded_by(separators());

    let branch = br_keyword.ignore_then(
        label_name()
            .map_with(|label, e| Terminator::Branch(SymbolId(e.state().intern(label.to_owned())))),
    );

    let branch_condition = br_keyword
        .ignore_then(variable_or_number())
        .then(label_name())
        .then(label_name())
        .map_with(
            |((cond, true_label), false_label), e| Terminator::BranchCondition {
                cond,
                true_label: SymbolId(e.state().intern(true_label.to_owned())),
                false_label: SymbolId(e.state().intern(false_label.to_owned())),
            },
        );

    let return_inst = return_keyword.to(Terminator::Return);

    let return_value = return_keyword
        .ignore_then(variable_or_number())
        .map(Terminator::ReturnValue);

    choice((branch, branch_condition, return_value, return_inst))
        .padded_by(comment().repeated())
        .padded()
}

fn basic_block<'src>() -> impl Parser<'src, &'src str, BasicBlock, IRExtra<'src>> {
    label_name()
        .padded_by(comment().repeated())
        .padded()
        .then(instruction().repeated().collect::<Vec<Instruction>>())
        .then(terminator())
        .map_with(|((label, instructions), terminator), e| BasicBlock {
            label: SymbolId(e.state().intern(label.to_owned())),
            instructions,
            terminator,
        })
        .padded_by(comment().repeated())
        .padded()
}

fn function<'src>() -> impl Parser<'src, &'src str, Function, IRExtra<'src>> {
    just("define")
        .padded_by(separators())
        .ignore_then(type_or_void())
        .then(function_name())
        .then_ignore(just('(').padded_by(separators()))
        .then(parameters())
        .then_ignore(just(')').padded_by(separators()))
        .then_ignore(just('{').padded_by(comment().repeated()).padded())
        .then(
            basic_block()
                .repeated()
                .at_least(1)
                .collect::<Vec<BasicBlock>>(),
        )
        .then_ignore(just('}').padded_by(comment().repeated()).padded())
        .map_with(|(((ty, name), params), basic_blocks), e| {
            let name = SymbolId(e.state().intern(name.to_owned()));
            let cfg = ControlFlowGraph::new(&basic_blocks);
            Function {
                ty,
                name,
                params,
                basic_blocks,
                cfg,
            }
        })
        .padded_by(comment().repeated())
        .padded()
}

fn program<'src>() -> impl Parser<'src, &'src str, Program, IRExtra<'src>> {
    function()
        .repeated()
        .at_least(1)
        .collect::<Vec<Function>>()
        .map_with(|functions, e| Program {
            functions,
            interner: mem::take(e.state()),
        })
}
