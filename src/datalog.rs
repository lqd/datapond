//! A module with helpers to work with datalog and datafrog: containing
//! - a simple data model to analyze, and compute rule transformations.
//! - a primitive parser for _valid_ syntax creating instances of the data model.
//! - a simple datalog-to-datafrog generator which will generate a skeleton
//! datafrog computation of the datalog rules, including preparing data in
//! `Relations`, the computed `Variables`, the join/antijoin/map operations
//! translations of the rules, and setup and maintenance of the indices used during
//! the joins and their possibly intermediate steps.

use std::fmt;
use std::ops::Deref;

/// Whether a predicate is used only as input, or produces new tuples.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum PredicateKind {
    Extensional,
    Intensional,
}

/// An atom, or relational atom, is a building block used in rules, also known as subgoal,
/// describing a relation name and the name of its components.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom<'a> {
    pub predicate: String,
    pub args: Vec<&'a str>,
}

/// A richer type of relation/atom, which can be negated, and used as premises/hypotheses in rules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal<'a> {
    pub atom: Atom<'a>,
    pub is_negated: bool,
    pub kind: PredicateKind,
}

/// A specific type of Horn clause relating the premises/hypotheses/antecedents/conditions in its body
/// to the conclusion/consequent in its head.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Rule<'a> {
    pub head: Atom<'a>,
    pub body: Vec<Literal<'a>>,
}

impl<'a> Atom<'a> {
    pub fn new(predicate: &'a str, args: Vec<&'a str>) -> Self {
        Atom {
            predicate: predicate.to_string(),
            args,
        }
    }
}

impl fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.predicate)?;
        for (idx, arg) in self.args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if idx < self.args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl<'a> Literal<'a> {
    pub fn new(predicate: &'a str, args: Vec<&'a str>) -> Self {
        Self {
            atom: Atom::new(predicate, args),
            is_negated: false,
            kind: PredicateKind::Extensional,
        }
    }

    pub fn new_anti(predicate: &'a str, args: Vec<&'a str>) -> Self {
        Self {
            atom: Atom::new(predicate, args),
            is_negated: true,
            kind: PredicateKind::Extensional,
        }
    }
}

impl<'a> Deref for Literal<'a> {
    type Target = Atom<'a>;

    fn deref(&self) -> &Self::Target {
        &self.atom
    }
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_negated {
            write!(f, "!")?;
        }
        write!(f, "{}", self.atom)
    }
}

impl fmt::Display for Rule<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :- ", self.head)?;
        for (idx, h) in self.body.iter().enumerate() {
            write!(f, "{}", h)?;
            if idx < self.body.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ".")
    }
}
