use crate::ast;
use crate::generator_new::ast as gen;

fn encode(program: ast::Program) -> gen::Iteration {
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
        let mut iter = rule.body.iter();
        let literal1 = iter.next().unwrap();
        if literal1.is_negated {
            unimplemented!();
        }
        let variable = iteration.get_variable(&literal1.predicate);
        let args = literal1.args.clone();

        // Retrieve the main variable for the head.
        // Create a new variable `res` for the rule result.
        // {
        //  if rule.body.len() == 1 {
        //      res.from_map(rule.body[0], ...)
        //  } else if rule.body.len() == 2 {
        //      let first = rule.body[0];
        //      let second = rule.body[1];
        //      let (key, first_remainder, second_remainder) = common_args(first, second);
        //      let new_first = variable::<(key, first_remainder)>;
        //      let new_second = variable::<(key, second_remainder)>;
        //      new_first.from_map(first);
        //      new_second.from_map(second);
        //      res.from_join(new_first, new_second, ...)
        //  }
        // }

        //
        // Extend the main variable with the new variable.
        // rule.head.extend(res);
        let head_variable = iteration.get_variable(&rule.head.predicate);
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

impl std::convert::From<Vec<ast::Arg>> for gen::DVars {
    fn from(args: Vec<ast::Arg>) -> Self {
        gen::DVars::new_tuple(args.into_iter().map(|arg| arg.to_ident()).collect())
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

    #[test]
    fn encode_simple1() {
        let program = parse(
            "
                input inp(x: u32, y: u32)
                output out(x: u32, y: u32)
                out(x, y) :- inp(y, x).
            ",
        );
        let program = typecheck(program).unwrap();
        let iteration = encode(program);
        let tokens = iteration.to_token_stream().to_string();
        eprintln!("{}", tokens);
        let expected_tokens = TokenStream::from_str(
            r##"
                let mut iteration = datafrog::Iteration::new();
                let inp = datafrog::Relation::from_vec:: <(u32, u32,)>(inp);
                let out = iteration.variable:: <(u32, u32,)>("out");
                while iteration.changed() {
                    out.from_map(&inp, | &(y, x,)| (x, y,));
                }
                let out = out.complete();
            "##,
        )
        .unwrap();
        assert_eq!(tokens.to_string(), expected_tokens.to_string());
    }
}
