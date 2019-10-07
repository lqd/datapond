use rustc_hash::FxHashMap;

use crate::generator::ArgDecl;
use crate::{Atom, Literal, Rule};

/// Primitive and inefficient parser for _valid_ datalog, with no error checking. Basically deserializing
/// a list of `Display` representations of a `Rule`, with a couple tweaks to make it easier
/// to write and use:
/// - ignores empty lines and the ones starting with `//`comments
/// - ignores whitespace between tokens
pub fn parse(text: &str) -> Vec<Rule<'_>> {
    let mut rules = Vec::new();

    for rule in text.split(".").map(|s| s.trim()).filter(|s| !s.is_empty()) {
        let parts: Vec<_> = rule.split(":-").map(|s| s.trim()).collect();
        let head = parts[0];
        let body = parts[1].split("),");

        let head = {
            let idx = head.find("(").unwrap();
            let predicate = &head[..idx];
            let args = &head[idx..];

            let start = args.find("(").unwrap() + 1;
            let end = args.find(")").unwrap();
            let args: Vec<_> = args[start..end].split(", ").collect();

            Atom::new(predicate, args)
        };

        let body = {
            let string_literals = body.map(|s| s.trim());
            let mut body = Vec::new();

            for literal in string_literals {
                let idx = literal.find("(").unwrap();
                let mut predicate = &literal[..idx];
                let mut args = &literal[idx..];

                let is_negated = {
                    if predicate.starts_with("!") {
                        predicate = &predicate[1..];
                        true
                    } else {
                        false
                    }
                };

                let start = args.find("(").unwrap() + 1;
                if let Some(end) = args.find(")") {
                    args = &args[start..end];
                } else {
                    args = &args[start..];
                }

                let args: Vec<_> = args.split(", ").collect();

                let literal = if is_negated {
                    Literal::new_anti(predicate, args)
                } else {
                    Literal::new(predicate, args)
                };

                body.push(literal);
            }

            body
        };

        let rule = Rule { head, body };
        rules.push(rule);
    }

    rules
}

// Primitive parser of relation declarations:
// - one-per line
// - the syntax is similar to Soufflé's decls:
//   `.decl $relation($arg_a: TypeA, $arg_b: TypeB, ...)`
//
// The goal is to map from a relation name to its canonical argument names and ordering,
// which you can't have solely in rules (where variables/arguments can have arbitrary names).
// This is used when generating skeleton datafrog computations, to help naming the
// relation indices by the canonical variable names used in the index key.
pub fn parse_declarations(decls: &str) -> FxHashMap<String, Vec<ArgDecl>> {
    let mut declarations = FxHashMap::default();
    for line in decls.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
        let prefix = ".decl ".len();
        let line = &line[prefix..];

        let idx = line.find("(").unwrap();
        let predicate = &line[..idx];
        let args = &line[idx..];

        let start = args.find("(").unwrap() + 1;
        let end = args.find(")").unwrap();
        let args: Vec<_> = args[start..end]
            .split(",")
            .map(|arg| {
                let mut typed_arg_decl = arg.trim().split(":");
                let name = typed_arg_decl.next().unwrap().to_lowercase();
                let rust_type = typed_arg_decl.next().unwrap().trim().to_string();

                ArgDecl { name, rust_type }
            })
            .collect();

        declarations.insert(predicate.to_string(), args);
    }
    declarations
}

pub fn clean_program(text: String) -> String {
    text.lines()
        .map(|s| s.trim())
        .filter(|line| !line.starts_with("//"))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_valid_datalog() {
        let program = parse("p(x, y) :- e(x, y). p(x, z) :- e(x, y), p(y, z).");
        assert_eq!("p(x, y) :- e(x, y).", program[0].to_string());
        assert_eq!("p(x, z) :- e(x, y), p(y, z).", program[1].to_string());
    }

    #[test]
    fn parse_multiline_datalog() {
        let text = r#"
            subset(O1, O2, P)    :- outlives(O1, O2, P).
            subset(O1, O3, P)    :- subset(O1, O2, P), subset(O2, O3, P).
            subset(O1, O2, Q)    :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
            requires(O, L, P)    :- borrow_region(O, L, P).
            requires(O2, L, P)   :- requires(O1, L, P), subset(O1, O2, P).
            requires(O, L, Q)    :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
            borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
            errors(L, P)         :- invalidates(L, P), borrow_live_at(L, P)."#;

        let program = parse(text);
        let serialized = program
            .into_iter()
            .map(|rule| rule.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        let expected = r#"subset(O1, O2, P) :- outlives(O1, O2, P).
subset(O1, O3, P) :- subset(O1, O2, P), subset(O2, O3, P).
subset(O1, O2, Q) :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
requires(O, L, P) :- borrow_region(O, L, P).
requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
errors(L, P) :- invalidates(L, P), borrow_live_at(L, P)."#;
        assert_eq!(expected, serialized);
    }

    #[test]
    fn parse_multiline_datalog_with_comments() {
        let text = r#"
            // `subset` rules
            subset(O1, O2, P) :- outlives(O1, O2, P).

            subset(O1, O3, P) :- subset(O1, O2, P),
                                   subset(O2, O3, P).
            subset(O1, O2, Q) :-
              subset(O1, O2, P),
              cfg_edge(P, Q),
              region_live_at(O1, Q),
              region_live_at(O2, Q).

            // `requires` rules
            requires(O, L, P) :- borrow_region(O, L, P).

            requires(O2, L, P) :-
              requires(O1, L, P),subset(O1, O2, P).

            requires(O, L, Q) :-
              requires(O, L, P),
                       !killed(L, P),    cfg_edge(P, Q),
    region_live_at(O, Q).

            // this one is commented out, nope(N, O, P, E) :- open(O, P, E, N).

            borrow_live_at(L, P) :-
              requires(O, L, P),
              region_live_at(O, P).

            errors(L, P) :-
              invalidates(L, P),
              borrow_live_at(L, P)."#;

        let program = clean_program(text.to_string());
        let rules = parse(&program);

        let serialized = rules
            .into_iter()
            .map(|rule| rule.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        let expected = r#"subset(O1, O2, P) :- outlives(O1, O2, P).
subset(O1, O3, P) :- subset(O1, O2, P), subset(O2, O3, P).
subset(O1, O2, Q) :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
requires(O, L, P) :- borrow_region(O, L, P).
requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
errors(L, P) :- invalidates(L, P), borrow_live_at(L, P)."#;
        assert_eq!(expected, serialized);
    }
}
