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

use crate::data_structures::OrderedMap;
use std::collections::HashMap;

/// A Datalog variable.
///
/// For example, `x` in the following:
/// ```ignore
/// r_1.from_map(&r, |(x, z)| {(z, x)});
/// ```
#[derive(Debug, Clone)]
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

impl DVarTuple {
    pub fn new(args: Vec<syn::Ident>) -> Self {
        Self {
            vars: args.into_iter().map(|ident| DVar::new(ident)).collect(),
        }
    }
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
        DVars::Tuple(DVarTuple::new(args))
    }
    pub fn new_key_val(key: Vec<syn::Ident>, value: Vec<syn::Ident>) -> Self {
        DVars::KeyVal(DVarKeyVal {
            key: key.into_iter().map(|ident| DVar::new(ident)).collect(),
            value: value.into_iter().map(|ident| DVar::new(ident)).collect(),
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

impl std::convert::From<Vec<syn::Type>> for DVarTypes {
    fn from(types: Vec<syn::Type>) -> Self {
        DVarTypes::Tuple(types)
    }
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

impl Variable {
    pub fn with_counter(&self, counter: usize) -> Self {
        Self {
            name: syn::Ident::new(
                &format!("{}_{}", self.name, counter),
                proc_macro2::Span::call_site(),
            ),
        }
    }
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
    pub output: Variable,
    /// A variable from which we read the input.
    pub input: Variable,
    /// Input variables that are copied to output and potentially used for evaluating `expr`.
    pub vars: DVarTuple,
    /// The expression whose result is bound to a new variable.
    pub expr: syn::Expr,
}

/// An operation that joins two variables.
#[derive(Debug)]
pub(crate) struct JoinOp {
    /// A variable into which we write the result.
    pub output: Variable,
    /// The first variable, which we use in join.
    pub input_first: Variable,
    /// The second variable, which we use in join.
    pub input_second: Variable,
    /// Datalog variables used for joining.
    pub key: DVarTuple,
    /// Datalog value variables from the first variable.
    pub value_first: DVarTuple,
    /// Datalog value variables from the second variable.
    pub value_second: DVarTuple,
}

/// An operation that filters out facts.
#[derive(Debug)]
pub(crate) struct FilterOp {
    /// A variable which we want to filter.
    pub variable: Variable,
    pub vars: DVars,
    /// A boolean expression used for filtering.
    pub expr: syn::Expr,
}

/// An operation that inserts the relation into a variable.
#[derive(Debug)]
pub(crate) struct InsertOp {
    /// The variable into which we want to insert the relation.
    pub variable: Variable,
    /// The relation to be inserted.
    pub relation: Variable,
}

#[derive(Debug)]
pub(crate) enum Operation {
    Reorder(ReorderOp),
    // BindVar(BindVarOp),
    Join(JoinOp),
    // Filter(FilterOp),
    Insert(InsertOp),
}

/// A Datafrog iteration.
#[derive(Debug)]
pub(crate) struct Iteration {
    /// Variables that are converted relations.
    relation_variables: HashMap<syn::Ident, syn::Ident>,
    pub relations: OrderedMap<syn::Ident, RelationDecl>,
    pub variables: OrderedMap<syn::Ident, VariableDecl>,
    /// Operations performed before entering the iteration.
    pub pre_operations: Vec<Operation>,
    /// Operations performed in the body of the iteration.
    pub body_operations: Vec<Operation>,
    /// Operations performed after exiting the iteration.
    pub post_operations: Vec<Operation>,
}

impl Iteration {
    pub fn new(relations: Vec<RelationDecl>, variables: Vec<VariableDecl>) -> Self {
        Self {
            relation_variables: HashMap::new(),
            relations: relations
                .into_iter()
                .map(|decl| (decl.var.name.clone(), decl))
                .collect(),
            variables: variables
                .into_iter()
                .map(|decl| (decl.var.name.clone(), decl))
                .collect(),
            pre_operations: Vec::new(),
            body_operations: Vec::new(),
            post_operations: Vec::new(),
        }
    }
    /// Convert a Datafrog relation to a Datafrog variable and return its identifier.
    pub fn convert_relation_to_variable(&mut self, variable: &Variable) -> Variable {
        if let Some(name) = self.relation_variables.get(&variable.name) {
            return self.variables[name].var.clone();
        }
        let decl = &self.relations[&variable.name];
        let variable_decl = VariableDecl {
            var: decl.var.with_counter(self.variables.len()),
            typ: decl.typ.clone().into(),
            is_output: false,
        };
        let new_variable = variable_decl.var.clone();
        self.relation_variables
            .insert(variable.name.clone(), new_variable.name.clone());
        self.variables
            .insert(new_variable.name.clone(), variable_decl);
        self.pre_operations.push(Operation::Insert(InsertOp {
            variable: new_variable.clone(),
            relation: decl.var.clone(),
        }));
        new_variable
    }
    /// Get Datafrog variable that corresponds to the given predicate name. If
    /// we have only a relation, then convert it into a variable.
    pub fn get_or_convert_variable(&mut self, predicate: &syn::Ident) -> Variable {
        if let Some(variable) = self.get_relation_var(predicate) {
            // TODO: Avoid converting the same relation multiple times.
            self.convert_relation_to_variable(&variable)
        } else {
            self.get_variable(predicate)
        }
    }
    pub fn get_relation_var(&self, variable_name: &syn::Ident) -> Option<Variable> {
        self.relations
            .get(variable_name)
            .map(|decl| decl.var.clone())
    }
    pub fn get_variable(&self, variable_name: &syn::Ident) -> Variable {
        self.variables[variable_name].var.clone()
    }
    pub fn add_operation(&mut self, operation: Operation) {
        self.body_operations.push(operation);
    }
    pub fn get_variable_tuple_types(&self, variable: &Variable) -> Vec<syn::Type> {
        let decl = &self.variables[&variable.name];
        match &decl.typ {
            DVarTypes::Tuple(types) => types.clone(),
            DVarTypes::KeyVal { .. } => unreachable!(),
        }
    }
    pub fn create_key_val_variable(
        &mut self,
        variable: &Variable,
        key: Vec<syn::Type>,
        value: Vec<syn::Type>,
    ) -> Variable {
        self.create_variable(variable, DVarTypes::KeyVal { key, value })
    }
    pub fn create_tuple_variable(
        &mut self,
        variable: &Variable,
        types: Vec<syn::Type>,
    ) -> Variable {
        self.create_variable(variable, DVarTypes::Tuple(types))
    }
    pub fn create_variable(&mut self, variable: &Variable, typ: DVarTypes) -> Variable {
        let variable_decl = VariableDecl {
            var: variable.with_counter(self.variables.len()),
            typ: typ,
            is_output: false,
        };
        let new_variable = variable_decl.var.clone();
        self.variables
            .insert(new_variable.name.clone(), variable_decl);
        new_variable
    }
}
