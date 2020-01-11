use crate::ast;
use crate::generator_new::ast as gen;

/// Divide the arguments into three sets:
///
/// 1. `key` – the arguments that are common in `first` and `second`.
/// 2. `first_remainder` – the arguments that are unique in `first`.
/// 3. `second_remainder` – the arguments that are unique in `second`.
fn common_args(
    first: &Vec<ast::Arg>,
    first_types: &Vec<syn::Type>,
    second: &Vec<ast::Arg>,
    second_types: &Vec<syn::Type>,
) -> (
    (Vec<ast::Arg>, Vec<syn::Type>),
    (Vec<ast::Arg>, Vec<syn::Type>),
    (Vec<ast::Arg>, Vec<syn::Type>),
) {
    assert!(first.len() == first_types.len());
    assert!(second.len() == second_types.len());

    let mut key = Vec::new();
    let mut key_types = Vec::new();
    let mut first_remainder = Vec::new();
    let mut first_remainder_types = Vec::new();

    for (arg1, arg1_type) in first.iter().zip(first_types) {
        if arg1.is_wildcard() {
            continue;
        }
        let mut found = false;
        for arg2 in second {
            if arg1 == arg2 {
                key.push(arg1.clone());
                key_types.push(arg1_type.clone());
                found = true;
                break;
            }
        }
        if !found {
            first_remainder.push(arg1.clone());
            first_remainder_types.push(arg1_type.clone());
        }
    }
    let mut second_remainder = Vec::new();
    let mut second_remainder_types = Vec::new();
    for (arg2, arg2_type) in second.iter().zip(second_types) {
        if arg2.is_wildcard() {
            continue;
        }
        if !key.contains(arg2) {
            second_remainder.push(arg2.clone());
            second_remainder_types.push(arg2_type.clone());
        }
    }

    (
        (key, key_types),
        (first_remainder, first_remainder_types),
        (second_remainder, second_remainder_types),
    )
}

pub(crate) fn encode(program: ast::Program) -> gen::Iteration {
    let mut relations = Vec::new();
    let mut variables = Vec::new();
    for decl in program.decls.values() {
        let var = gen::Variable {
            name: decl.name.clone(),
        };
        let typ = decl
            .parameters
            .iter()
            .map(|param| param.typ.clone())
            .collect();
        match decl.kind {
            ast::PredicateKind::Input => {
                relations.push(gen::RelationDecl { var: var, typ: typ });
            }
            ast::PredicateKind::Internal => {
                variables.push(gen::VariableDecl {
                    var: var,
                    typ: gen::DVarTypes::Tuple(typ),
                    is_output: false,
                });
            }
            ast::PredicateKind::Output => {
                variables.push(gen::VariableDecl {
                    var: var,
                    typ: gen::DVarTypes::Tuple(typ),
                    is_output: true,
                });
            }
        }
    }
    let mut iteration = gen::Iteration::new(relations, variables);
    for rule in &program.rules {
        let head_variable = iteration.get_variable(&rule.head.predicate);
        let mut iter = rule.body.iter();
        let literal1 = iter.next().unwrap();
        assert!(!literal1.is_negated);
        let mut variable = iteration.get_or_convert_variable(&literal1.predicate);
        let mut args = literal1.args.clone();

        while let Some(literal) = iter.next() {
            // TODO: Check during the typechecking phase that no literal has two
            // arguments with the same name.
            let (new_variable, new_args) = if literal.is_negated {
                encode_antijoin(&mut iteration, &head_variable, variable, args, literal)
            } else {
                encode_join(&mut iteration, &head_variable, variable, args, literal)
            };
            variable = new_variable;
            args = new_args;
        }
        let reorder_op = gen::ReorderOp {
            output: head_variable,
            input: variable,
            input_vars: args.into(),
            output_vars: rule.head.args.clone().into(),
        };
        iteration.add_operation(gen::Operation::Reorder(reorder_op));
    }
    iteration
}

fn encode_antijoin(
    iteration: &mut gen::Iteration,
    head_variable: &gen::Variable,
    variable: gen::Variable,
    args: Vec<ast::Arg>,
    literal: &ast::Literal,
) -> (gen::Variable, Vec<ast::Arg>) {
    let relation_variable = iteration
        .get_relation_var(&literal.predicate)
        .expect("Negations are currently supported only on relations.");
    let arg_types = iteration.get_variable_tuple_types(&variable);
    let literal_arg_types = iteration.relations[&relation_variable.name].typ.clone();

    // TODO: Lift this limitation.
    for arg in &literal.args {
        if !args.contains(arg) {
            unimplemented!("Currently all variables from the negated relation must be used.");
        }
    }
    let mut remainder = Vec::new();
    let mut remainder_types = Vec::new();
    for (arg, arg_type) in args.iter().zip(&arg_types) {
        if !literal.args.contains(arg) {
            remainder.push(arg.clone());
            remainder_types.push(arg_type.clone());
        }
    }

    let first_variable = iteration.create_key_val_variable(
        &variable,
        literal_arg_types.clone(),
        remainder_types.clone(),
    );
    let reorder_first_op = gen::ReorderOp {
        output: first_variable.clone(),
        input: variable,
        input_vars: args.into(),
        output_vars: (literal.args.clone(), remainder.clone()).into(),
    };
    iteration.add_operation(gen::Operation::Reorder(reorder_first_op));

    let result_types = literal_arg_types
        .into_iter()
        .chain(remainder_types)
        .collect();
    let args = literal
        .args
        .clone()
        .into_iter()
        .chain(remainder.clone())
        .collect();
    let variable = iteration.create_tuple_variable(&head_variable, result_types);
    let join_op = gen::AntiJoinOp {
        output: variable.clone(),
        input_variable: first_variable,
        input_relation: relation_variable,
        key: literal.args.clone().into(),
        value: remainder.into(),
    };
    iteration.add_operation(gen::Operation::AntiJoin(join_op));
    (variable, args)
}

fn encode_join(
    iteration: &mut gen::Iteration,
    head_variable: &gen::Variable,
    variable: gen::Variable,
    args: Vec<ast::Arg>,
    literal: &ast::Literal,
) -> (gen::Variable, Vec<ast::Arg>) {
    let joined_variable = iteration.get_or_convert_variable(&literal.predicate);
    let arg_types = iteration.get_variable_tuple_types(&variable);
    let literal_arg_types = iteration.get_variable_tuple_types(&joined_variable);
    let ((key, key_types), (remainder1, remainder1_types), (remainder2, remainder2_types)) =
        common_args(&args, &arg_types, &literal.args, &literal_arg_types);
    let first_variable =
        iteration.create_key_val_variable(&variable, key_types.clone(), remainder1_types.clone());
    let reorder_first_op = gen::ReorderOp {
        output: first_variable.clone(),
        input: variable,
        input_vars: args.into(),
        output_vars: (key.clone(), remainder1.clone()).into(),
    };
    iteration.add_operation(gen::Operation::Reorder(reorder_first_op));
    let second_variable = iteration.create_key_val_variable(
        &joined_variable,
        key_types.clone(),
        remainder2_types.clone(),
    );
    let reorder_second_op = gen::ReorderOp {
        output: second_variable.clone(),
        input: joined_variable,
        input_vars: literal.args.clone().into(),
        output_vars: (key.clone(), remainder2.clone()).into(),
    };
    iteration.add_operation(gen::Operation::Reorder(reorder_second_op));
    let result_types = key_types
        .into_iter()
        .chain(remainder1_types)
        .chain(remainder2_types)
        .collect();
    let args = key
        .clone()
        .into_iter()
        .chain(remainder1.clone())
        .chain(remainder2.clone())
        .collect();
    let variable = iteration.create_tuple_variable(&head_variable, result_types);
    let join_op = gen::JoinOp {
        output: variable.clone(),
        input_first: first_variable,
        input_second: second_variable,
        key: key.into(),
        value_first: remainder1.into(),
        value_second: remainder2.into(),
    };
    iteration.add_operation(gen::Operation::Join(join_op));
    (variable, args)
}

impl std::convert::From<Vec<ast::Arg>> for gen::DVars {
    fn from(args: Vec<ast::Arg>) -> Self {
        gen::DVars::new_tuple(args.into_iter().map(|arg| arg.to_ident()).collect())
    }
}

impl std::convert::From<Vec<ast::Arg>> for gen::DVarTuple {
    fn from(args: Vec<ast::Arg>) -> Self {
        gen::DVarTuple::new(args.into_iter().map(|arg| arg.to_ident()).collect())
    }
}

impl std::convert::From<(Vec<ast::Arg>, Vec<ast::Arg>)> for gen::DVars {
    fn from((key, value): (Vec<ast::Arg>, Vec<ast::Arg>)) -> Self {
        gen::DVars::new_key_val(
            key.into_iter().map(|arg| arg.to_ident()).collect(),
            value.into_iter().map(|arg| arg.to_ident()).collect(),
        )
    }
}

impl std::convert::From<Vec<syn::Ident>> for gen::DVars {
    fn from(args: Vec<syn::Ident>) -> Self {
        gen::DVars::new_tuple(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::typechecker::typecheck;
    use proc_macro2::TokenStream;
    use quote::ToTokens;
    use std::str::FromStr;

    fn compare(datalog_source: &str, exptected_encoding: &str) {
        let parsed_program = parse(datalog_source);
        let typechecked_program = typecheck(parsed_program).unwrap();
        let iteration = encode(typechecked_program);
        let tokens = iteration.to_token_stream().to_string();
        eprintln!("{}", tokens);
        let expected_tokens = TokenStream::from_str(exptected_encoding).unwrap();
        assert_eq!(tokens.to_string(), expected_tokens.to_string());
    }

    #[test]
    fn encode_simple1() {
        compare(
            "
                input inp(x: u32, y: u32)
                output out(x: u32, y: u32)
                out(x, y) :- inp(y, x).
            ",
            r##"
                {
                    let mut iteration = datafrog::Iteration::new();
                    let var_inp = datafrog::Relation:: <(u32, u32,)> ::from_vec(inp);
                    let var_out = iteration.variable:: <(u32, u32,)>("out");
                    let var_inp_1 = iteration.variable:: <(u32, u32,)>("inp_1");
                    var_inp_1.insert(var_inp);
                    while iteration.changed() {
                        var_out.from_map(&var_inp_1, | &(y, x,)| (x, y,));
                    }
                    out = var_out.complete();
                }
            "##,
        );
    }
    #[test]
    fn encode_transitive_closure() {
        compare(
            "
                input inp(x: u32, y: u32)
                output out(x: u32, y: u32)
                out(x, y) :- inp(x, y).
                out(x, y) :- out(x, z), out(z, y).
            ",
            r##"
                {
                    let mut iteration = datafrog::Iteration::new();
                    let var_inp = datafrog::Relation:: <(u32, u32,)> ::from_vec(inp);
                    let var_out = iteration.variable:: <(u32, u32,)>("out");
                    let var_inp_1 = iteration.variable:: <(u32, u32,)>("inp_1");
                    let var_out_2 = iteration.variable:: <((u32,), (u32,))>("out_2");
                    let var_out_3 = iteration.variable:: <((u32,), (u32,))>("out_3");
                    let var_out_4 = iteration.variable:: <(u32, u32, u32,)>("out_4");
                    var_inp_1.insert(var_inp);
                    while iteration.changed() {
                        var_out.from_map(&var_inp_1, | &(x, y,)| (x, y,));
                        var_out_2.from_map(&var_out, | &(x, z,)| ((z,), (x,)));
                        var_out_3.from_map(&var_out, | &(z, y,)| ((z,), (y,)));
                        var_out_4.from_join(&var_out_2, &var_out_3, | &(z,), &(x,), &(y,)| (z, x, y,));
                        var_out.from_map(&var_out_4, | &(z, x, y,)| (x, y,));
                    }
                    out = var_out.complete();
                }
            "##,
        );
    }
    #[test]
    fn encode_rule_with_wildcards() {
        compare(
            "
                input inp(x: u32, y: u32)
                output out(x: u32)
                out(x) :- inp(x, _), inp(_, x).
            ",
            r##"
                {
                    let mut iteration = datafrog::Iteration::new();
                    let var_inp = datafrog::Relation:: <(u32, u32,)> ::from_vec(inp);
                    let var_out = iteration.variable:: <(u32,)>("out");
                    let var_inp_1 = iteration.variable:: <(u32, u32,)>("inp_1");
                    let var_inp_1_2 = iteration.variable:: <((u32,), ())>("inp_1_2");
                    let var_inp_1_3 = iteration.variable:: <((u32,), ())>("inp_1_3");
                    let var_out_4 = iteration.variable:: <(u32,)>("out_4");
                    var_inp_1.insert(var_inp);
                    while iteration.changed() {
                        var_inp_1_2.from_map(&var_inp_1, | &(x, _,)| ((x,), ()));
                        var_inp_1_3.from_map(&var_inp_1, | &(_, x,)| ((x,), ()));
                        var_out_4.from_join(&var_inp_1_2, &var_inp_1_3, | &(x,), &(), &()| (x,));
                        var_out.from_map(&var_out_4, | &(x,)| (x,));
                    }
                    out = var_out.complete();
                }
            "##,
        );
    }
    #[test]
    fn encode_kill() {
        compare(
            "
                input inp(x: u32, y: u32)
                input kill(y: u32)
                output out(x: u32, y: u32)
                out(x, y) :- inp(x, y), !kill(y).
            ",
            r##"
                {
                    let mut iteration = datafrog::Iteration::new();
                    let var_inp = datafrog::Relation:: <(u32, u32,)> ::from_vec(inp);
                    let var_kill = datafrog::Relation:: <(u32,)> ::from_vec(kill);
                    let var_out = iteration.variable:: <(u32, u32,)>("out");
                    let var_inp_1 = iteration.variable:: <(u32, u32,)>("inp_1");
                    let var_inp_1_2 = iteration.variable:: <((u32,), (u32,))>("inp_1_2");
                    let var_out_3 = iteration.variable:: <(u32, u32,)>("out_3");
                    var_inp_1.insert(var_inp);
                    while iteration.changed() {
                        var_inp_1_2.from_map(&var_inp_1, | &(x, y,)| ((y,), (x,)));
                        var_out_3.from_antijoin(&var_inp_1_2, &var_kill, | &(y,), &(x,)| (y, x,));
                        var_out.from_map(&var_out_3, | &(y, x,)| (x, y,));
                    }
                    out = var_out.complete();
                }
            "##,
        );
    }
}
