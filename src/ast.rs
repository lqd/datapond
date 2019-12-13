//! This file contains the typed AST.

use proc_macro2::Ident;
use quote::ToTokens;
use std::collections::HashMap;
use std::fmt;

/// The predicate kind regarding IO.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum PredicateKind {
    /// Instances of this predicate can be provided only as input facts.
    Input,
    /// Instances of this predicate can be used for computation but cannot be output.
    Internal,
    /// Instances of this predicate can be used for computation and also can be output.
    Output,
}

impl fmt::Display for PredicateKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PredicateKind::Input => write!(f, "input"),
            PredicateKind::Internal => write!(f, "internal"),
            PredicateKind::Output => write!(f, "output"),
        }
    }
}

/// Parameter information of the predicate declaration.
#[derive(Clone)]
pub struct ParamDecl {
    pub name: Ident,
    pub typ: syn::Type,
}

impl fmt::Debug for ParamDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ.to_token_stream())
    }
}

impl fmt::Display for ParamDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ.to_token_stream())
    }
}

impl PartialEq for ParamDecl {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for ParamDecl {}

impl ParamDecl {
    pub fn typ_as_string(&self) -> String {
        self.typ.to_token_stream().to_string()
    }
}

/// A declaration of the predicate.
///
/// ```plain
/// input Input(x: u32, y: u32)
/// internal Internal(x: u32, y: u32)
/// output Output(x: u32, y: u32)
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PredicateDecl {
    pub kind: PredicateKind,
    pub name: Ident,
    pub parameters: Vec<ParamDecl>,
}

impl fmt::Display for PredicateDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}(", self.kind, self.name)?;
        let mut first = true;
        for parameter in &self.parameters {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", parameter)?;
        }
        write!(f, ")")
    }
}

/// An argument.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    /// Identifier `arg`.
    Ident(Ident),
    /// Wildcard argument.
    Wildcard,
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::Ident(ident) => write!(f, "{}", ident),
            Arg::Wildcard => write!(f, "_"),
        }
    }
}

/// A richer type of atom, which can be negated, and used as
/// premises/hypotheses in rules.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub is_negated: bool,
    pub predicate: Ident,
    pub args: Vec<Arg>,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_negated {
            write!(f, "!")?;
        }
        write!(f, "{}(", self.predicate)?;
        let mut first = true;
        for arg in &self.args {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

/// A head of a rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleHead {
    pub predicate: Ident,
    pub args: Vec<Ident>,
}

impl fmt::Display for RuleHead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.predicate)?;
        let mut first = true;
        for arg in &self.args {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

/// A rule describing how to derive new facts.
///
/// ```plain
/// Internal(x, y) :- Input(x, y).
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
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

/// A Datalog program.
#[derive(Debug, Clone)]
pub struct Program {
    pub decls: HashMap<String, PredicateDecl>,
    pub rules: Vec<Rule>,
}
