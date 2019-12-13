//! This file contains the parse AST.

use proc_macro2::Ident;
use std::fmt;

pub(crate) use crate::ast::{ParamDecl, PredicateDecl, PredicateKind, RuleHead};

/// A positional argument `arg2`.
#[derive(Debug, Clone)]
pub(crate) enum PositionalArg {
    Ident(Ident),
    Wildcard,
}

impl fmt::Display for PositionalArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PositionalArg::Ident(ident) => write!(f, "{}", ident),
            PositionalArg::Wildcard => write!(f, "_"),
        }
    }
}

/// A named argument `.param2=arg2`.
#[derive(Debug, Clone)]
pub(crate) struct NamedArg {
    pub param: Ident,
    pub arg: Ident,
}

/// The list of atom's arguments.
#[derive(Debug, Clone)]
pub(crate) enum ArgList {
    /// arg1, arg2
    Positional(Vec<PositionalArg>),
    /// .param1=arg1, .param2=arg2
    Named(Vec<NamedArg>),
}

impl fmt::Display for ArgList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        match self {
            ArgList::Positional(args) => {
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
            }
            ArgList::Named(args) => {
                for kwarg in args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, ".{}={}", kwarg.param, kwarg.arg)?;
                }
            }
        }
        Ok(())
    }
}

/// A richer type of atom, which can be negated, and used as
/// premises/hypotheses in rules.
#[derive(Debug, Clone)]
pub(crate) struct Literal {
    pub is_negated: bool,
    pub predicate: Ident,
    pub args: ArgList,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_negated {
            write!(f, "!")?;
        }
        write!(f, "{}({})", self.predicate, self.args)
    }
}

/// A rule describing how to compute facts.
///
/// ```plain
/// Internal(x, y) :- Input(x, y).
/// ```
#[derive(Debug, Clone)]
pub(crate) struct Rule {
    pub head: RuleHead,
    pub body: Vec<Literal>,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} :- ", self.head)?;
        let mut first = true;
        for literal in &self.body {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", literal)?;
        }
        write!(f, ".")
    }
}

/// Items present in the program.
#[derive(Debug, Clone)]
pub(crate) enum ProgramItem {
    PredicateDecl(PredicateDecl),
    Rule(Rule),
}

impl fmt::Display for ProgramItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ProgramItem::PredicateDecl(decl) => write!(f, "{}", decl),
            ProgramItem::Rule(rule) => write!(f, "{}", rule),
        }
    }
}

/// A Datalog program.
#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub items: Vec<ProgramItem>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for item in &self.items {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}
