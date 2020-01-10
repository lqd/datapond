//! # Examples
//!
//! ## Example 1
//!
//! ```Datalog
//! input in(x: u32, y: u32);
//! output r(x: u32, y: u32);
//! r(x, y) = in(y, x);
//! ```
//! ``in`` is assumed to be a variable of type ``&Vec<(u32, u32)>``.
//! ```ignore
//! let r = in.iter().map(|(y, x)| {(x, y)});
//! ```
//!
//! ## Example 2
//!
//! ```Datalog
//! input in(x: u32, y: u32);
//! output r(x: u32, y: u32);
//! r(x, y) = in(y, x);
//! r(x, y) = r(x, z), r(z, y);
//! ```
//! ``in`` is assumed to be a variable of type ``&Vec<(u32, u32)>``.
//! ```ignore
//! let mut iteration = Iteration::new();
//! let r = iteration.variable::<(u32, u32)>("r");
//! let r_1 = iteration.variable::<(u32, u32)>("r_1");
//! let r_2 = iteration.variable::<(u32, u32)>("r_2");
//! while iteration.changed() {
//!     r_1.from_map(&r, |(x, z)| {(z, x)});
//!     r_2.from_map(&r, |(z, y)| {(z, y)});
//!     r.from_join(&r_1, &r_2, |(z, x, y)| {z, x, y});
//! }
//! let r = in.iter().map(|(y, x)| {(x, y)});
//! ```

use std::collections::HashMap;

/// A Datalog variable.
///
/// For example, `x` in the following:
/// ```ignore
/// r_1.from_map(&r, |(x, z)| {(z, x)});
/// ```
#[derive(Debug)]
pub(crate) struct DVar {
    pub name: syn::Ident,
}

impl DVar {
    pub fn new(name: syn::Ident) -> Self {
        Self { name: name }
    }
}

/// A flat tuple of `DVar`s. Typically used to represent the user defined types.
#[derive(Debug)]
pub(crate) struct DVarTuple {
    pub vars: Vec<DVar>,
}

/// A (key, value) representation of `DVar`s. It is used for joins.
#[derive(Debug)]
pub(crate) struct DVarKeyVal {
    pub key: Vec<DVar>,
    pub value: Vec<DVar>,
}

/// An ordered set of `DVar`s.
#[derive(Debug)]
pub(crate) enum DVars {
    Tuple(DVarTuple),
    KeyVal(DVarKeyVal),
}

impl DVars {
    pub fn new_tuple(args: Vec<syn::Ident>) -> Self {
        DVars::Tuple(DVarTuple {
            vars: args.into_iter().map(|ident| DVar::new(ident)).collect(),
        })
    }
}

/// A type that matches some `DVars`.
#[derive(Debug)]
pub(crate) enum DVarTypes {
    Tuple(Vec<syn::Type>),
    KeyVal {
        key: Vec<syn::Type>,
        value: Vec<syn::Type>,
    },
}

/// A Datafrog relation.
#[derive(Debug)]
pub(crate) struct RelationDecl {
    pub var: Variable,
    pub typ: Vec<syn::Type>,
}

/// A Datafrog variable.
///
/// For example, `rule` in the following:
/// ```ignore
/// let rule = iteration.variable::<(u32, u32)>("rule");
/// ```
#[derive(Debug)]
pub(crate) struct VariableDecl {
    pub var: Variable,
    /// The type by shape must match `DVarKeyVal`.
    pub typ: DVarTypes,
    pub is_output: bool,
}

/// A reference to a Datafrog relation or variable.
#[derive(Debug, Clone)]
pub(crate) struct Variable {
    pub name: syn::Ident,
}

/// An operation that reorders and potentially drops Datalog variables.
///
/// It is encoded as a Datafrog `from_map`.
#[derive(Debug)]
pub(crate) struct ReorderOp {
    /// A variable into which we write the result.
    pub output: Variable,
    /// A variable from which we read the input.
    pub input: Variable,
    pub input_vars: DVars,
    pub output_vars: DVars,
}

/// An operation that evaluates the given expression and adds it as a last output variable.
#[derive(Debug)]
pub(crate) struct BindVarOp {
    /// A variable into which we write the result.
    output: Variable,
    /// A variable from which we read the input.
    input: Variable,
    /// Input variables that are copied to output and potentially used for evaluating `expr`.
    vars: DVarTuple,
    /// The expression whose result is bound to a new variable.
    expr: syn::Expr,
}

/// An operation that joins two variables.
#[derive(Debug)]
pub(crate) struct JoinOp {
    /// A variable into which we write the result.
    output: Variable,
    /// The first variable, which we use in join.
    input_first: Variable,
    /// The second variable, which we use in join.
    input_second: Variable,
    /// Datalog variables used for joining.
    key: Vec<DVar>,
    /// Datalog value variables from the first variable.
    value_first: Vec<DVar>,
    /// Datalog value variables from the second variable.
    value_second: Vec<DVar>,
}

/// An operation that filters out facts.
#[derive(Debug)]
pub(crate) struct FilterOp {
    /// A variable which we want to filter.
    variable: Variable,
    vars: DVars,
    /// A boolean expression used for filtering.
    expr: syn::Expr,
}

#[derive(Debug)]
pub(crate) enum Operation {
    Reorder(ReorderOp),
    BindVar(BindVarOp),
    Join(JoinOp),
    Filter(FilterOp),
}

/// A Datafrog iteration.
#[derive(Debug)]
pub(crate) struct Iteration {
    pub relations: HashMap<syn::Ident, RelationDecl>,
    pub variables: HashMap<syn::Ident, VariableDecl>,
    pub operations: Vec<Operation>,
}

impl Iteration {
    pub fn new(relations: Vec<RelationDecl>, variables: Vec<VariableDecl>) -> Self {
        Self {
            relations: relations
                .into_iter()
                .map(|decl| (decl.var.name.clone(), decl))
                .collect(),
            variables: variables
                .into_iter()
                .map(|decl| (decl.var.name.clone(), decl))
                .collect(),
            operations: Vec::new(),
        }
    }
    pub fn get_variable(&self, variable_name: &syn::Ident) -> Variable {
        if let Some(decl) = self.variables.get(variable_name) {
            decl.var.clone()
        } else {
            self.relations[variable_name].var.clone()
        }
    }
    pub fn add_operation(&mut self, operation: Operation) {
        self.operations.push(operation);
    }
}
