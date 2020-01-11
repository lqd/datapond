use proc_macro2::Ident;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, punctuated::Punctuated, Token};

pub(crate) mod ast;

mod kw {
    syn::custom_keyword!(internal);
    syn::custom_keyword!(input);
    syn::custom_keyword!(output);
}

impl Parse for ast::PredicateKind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::input) {
            input.parse::<kw::input>()?;
            Ok(ast::PredicateKind::Input)
        } else if lookahead.peek(kw::internal) {
            input.parse::<kw::internal>()?;
            Ok(ast::PredicateKind::Internal)
        } else if lookahead.peek(kw::output) {
            input.parse::<kw::output>()?;
            Ok(ast::PredicateKind::Output)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ast::ParamDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let typ = input.parse()?;
        Ok(ast::ParamDecl { name, typ })
    }
}

impl Parse for ast::PredicateDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let kind = input.parse()?;
        let name = input.parse()?;
        let content;
        parenthesized!(content in input);
        let parsed_content: Punctuated<ast::ParamDecl, Token![,]> =
            content.parse_terminated(ast::ParamDecl::parse)?;
        let parameters = parsed_content
            .into_pairs()
            .map(|pair| pair.into_value())
            .collect();
        Ok(ast::PredicateDecl {
            kind,
            name,
            parameters,
        })
    }
}

impl Parse for ast::NamedArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![.]>()?;
        let param: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let arg: Ident = input.parse()?;
        Ok(ast::NamedArg { param, arg })
    }
}

impl Parse for ast::PositionalArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            Ok(ast::PositionalArg::Wildcard)
        } else {
            let ident = input.parse()?;
            Ok(ast::PositionalArg::Ident(ident))
        }
    }
}

impl Parse for ast::ArgList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);
        if content.peek(Token![.]) {
            let punctuated: Punctuated<ast::NamedArg, Token![,]> =
                content.parse_terminated(ast::NamedArg::parse)?;
            let args = punctuated
                .into_pairs()
                .map(|pair| pair.into_value())
                .collect();
            Ok(ast::ArgList::Named(args))
        } else {
            let punctuated: Punctuated<ast::PositionalArg, Token![,]> =
                content.parse_terminated(ast::PositionalArg::parse)?;
            let args = punctuated
                .into_pairs()
                .map(|pair| pair.into_value())
                .collect();
            Ok(ast::ArgList::Positional(args))
        }
    }
}

impl Parse for ast::Literal {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let is_negated = input.peek(Token![!]);
        if is_negated {
            input.parse::<Token![!]>()?;
        }
        let predicate = input.parse()?;
        let args = input.parse()?;
        Ok(ast::Literal {
            is_negated,
            predicate,
            args,
        })
    }
}

impl Parse for ast::RuleHead {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let predicate = input.parse()?;
        let content;
        parenthesized!(content in input);
        let punctuated: Punctuated<Ident, Token![,]> = content.parse_terminated(Ident::parse)?;
        let args = punctuated
            .into_pairs()
            .map(|pair| pair.into_value())
            .collect();
        Ok(ast::RuleHead { predicate, args })
    }
}

impl Parse for ast::Rule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let head = input.parse()?;
        // FIXME: For some reason, when getting input from a procedural macro,
        // a space is always inserted between `:` and `-`. Therefore, the parser
        // needs to accept the variant with a space.
        input.parse::<Token![:]>()?;
        input.parse::<Token![-]>()?;
        // input.step(|cursor| {
        //     let rest = match cursor.token_tree() {
        //         Some((proc_macro2::TokenTree::Punct(ref punct), next))
        //             if punct.as_char() == ':' && punct.spacing() == proc_macro2::Spacing::Joint =>
        //         {
        //             next
        //         }
        //         _ => return Err(cursor.error(":- expected")),
        //     };
        //     match rest.token_tree() {
        //         Some((proc_macro2::TokenTree::Punct(ref punct), next))
        //             if punct.as_char() == '-' =>
        //         {
        //             Ok(((), next))
        //         }
        //         _ => Err(cursor.error(":- expected")),
        //     }
        // })?;
        let body: Punctuated<ast::Literal, Token![,]> =
            Punctuated::parse_separated_nonempty(input)?;
        // Allow trailing punctuation.
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }
        input.parse::<Token![.]>()?;
        Ok(ast::Rule {
            head,
            body: body.into_pairs().map(|pair| pair.into_value()).collect(),
        })
    }
}

impl Parse for ast::Program {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::internal)
                || lookahead.peek(kw::input)
                || lookahead.peek(kw::output)
            {
                let decl: ast::PredicateDecl = input.parse()?;
                items.push(ast::ProgramItem::PredicateDecl(decl));
            } else {
                let rule: ast::Rule = input.parse()?;
                items.push(ast::ProgramItem::Rule(rule));
            }
        }
        Ok(ast::Program { items })
    }
}

/// Parse a Datalog program.
pub(crate) fn parse(text: &str) -> ast::Program {
    info!("parse text: {}", text);
    match syn::parse_str(text) {
        Ok(program) => program,
        Err(err) => panic!("Error: {:?} (at {:?})", err, err.span().start()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_relation_decl1() {
        let program = parse("input P ( x: u32   , y: u64)");
        assert_eq!(program.items.len(), 1);
        assert_eq!(program.to_string(), "input P(x: u32, y: u64)\n");
    }

    #[test]
    fn parse_relation_decl2() {
        let program = parse("internal P ( x: u32   , y: u64,)");
        assert_eq!(program.items.len(), 1);
        assert_eq!(program.to_string(), "internal P(x: u32, y: u64)\n");
        let program = parse("output P ( )");
        assert_eq!(program.items.len(), 1);
        assert_eq!(program.to_string(), "output P()\n");
    }

    #[test]
    fn parse_rule1() {
        let program = parse("P ( x   , y,) :-   Q( x, y), O(y, x) .");
        assert_eq!(program.items.len(), 1);
        assert_eq!(program.to_string(), "P(x, y) :- Q(x, y), O(y, x).\n");
    }

    #[test]
    fn parse_rule2() {
        let program = parse("P ( x   , y,) :-   Q( x, y), O(y, _) .");
        assert_eq!(program.items.len(), 1);
        assert_eq!(program.to_string(), "P(x, y) :- Q(x, y), O(y, _).\n");
    }

    #[test]
    fn parse_rule_trailing_comma() {
        let program = parse("P ( x   , y,) :-   Q( x, y), O(y, x) .");
        assert_eq!(program.items.len(), 1);
        assert_eq!(program.to_string(), "P(x, y) :- Q(x, y), O(y, x).\n");
    }

    #[test]
    fn parse_valid_datalog() {
        let program = parse(
            "
                input E(x: u32, y: u64)
                internal P(x: u32, y: u64)
                P(x, y) :- E(x, y).
                P(x, z) :- E(x, y), P(y, z).
            ",
        );
        assert_eq!(program.items.len(), 4);
        assert_eq!("input E(x: u32, y: u64)", program.items[0].to_string());
        assert_eq!("internal P(x: u32, y: u64)", program.items[1].to_string());
        assert_eq!("P(x, y) :- E(x, y).", program.items[2].to_string());
        assert_eq!("P(x, z) :- E(x, y), P(y, z).", program.items[3].to_string());
    }

    #[test]
    fn parse_named_args() {
        let program = parse(
            "
                internal P(x: u32, y: u64)
                p(x, y) :- e(.field1 = x, .field2 = y).
            ",
        );
        assert_eq!("internal P(x: u32, y: u64)", program.items[0].to_string());
        assert_eq!(
            "p(x, y) :- e(.field1=x, .field2=y).",
            program.items[1].to_string()
        );
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
            .items
            .into_iter()
            .map(|item| item.to_string())
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

        let items = parse(&text).items;

        let serialized = items
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
