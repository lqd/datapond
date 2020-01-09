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
//! ```rust
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
//! ```rust
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

/// A Datalog variable.
///
/// For example, `x` in the following:
/// ```rust
/// r_1.from_map(&r, |(x, z)| {(z, x)});
/// ```
struct DVar {
    name: syn::Ident,
}

/// A flat tuple of `DVar`s. Typically used to represent the user defined types.
struct DVarTuple {
    vars: Vec<DVar>,
}

/// A (key, value) representation of `DVar`s. It is used for joins.
struct DVarKeyVal {
    key: Vec<DVar>,
    value: Vec<DVar>,
}

/// An ordered set of `DVar`s.
enum DVars {
    Tuple(DVarTuple),
    KeyVal(DVarKeyVal),
}

/// A Datafrog variable.
///
/// For example, `rule` in the following:
/// ```rust
/// let rule = iteration.variable::<(u32, u32)>("rule");
/// ```
struct Variable {
    name: syn::Ident,
}

struct VariableDecl {
    var: Variable,
    typ: syn::Type,
    is_output: bool,
}

/// An operation that reorders and potentially drops Datalog variables.
///
/// It is encoded as a Datafrog `from_map`.
struct ReorderOp {
    /// A variable into which we write the result.
    output: Variable,
    /// A variable from which we read the input.
    input: Variable,
    input_vars: DVars,
    output_vars: DVars,
}

/// An operation that evaluates the given expression and adds it as a last output variable.
struct BindVarOp {
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
struct JoinOp {
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
struct FilterOp {
    /// A variable which we want to filter.
    variable: Variable,
    vars: DVars,
    /// A boolean expression used for filtering.
    expr: syn::Expr,
}

enum Operation {
    Reorder(ReorderOp),
    BindVar(BindVarOp),
    Join(JoinOp),
    Filter(FilterOp),
}

/// A Datafrog iteration.
struct Iteration {
    variables: Vec<VariableDecl>,
    operations: Vec<Operation>,
}
