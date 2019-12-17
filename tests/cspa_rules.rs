use datapond::generate_skeleton_datafrog;
use pretty_assertions::assert_eq;

#[test]
fn generate_cspa_rules() {
    // Context-sensitive Points-to Analysis rules
    // adapted from http://pages.cs.wisc.edu/~aws/papers/vldb19.pdf p. 9

    let text = r#"
        input assign(a: u32, b: u32)
        input dereference(a: u32, b: u32)
        
        internal value_flow(a: u32, b: u32)
        internal memory_alias(a: u32, b: u32)
        internal value_alias(a: u32, b: u32)

        value_flow(y, x) :- assign(y, x).
        value_flow(x, y) :- memory_alias(z, y), assign(x, z).
        value_flow(x, y) :- value_flow(x, z), value_flow(z, y).

        memory_alias(x, w) :- value_alias(y, z), dereference(y, x), dereference(z, w).

        value_alias(x, y) :- value_flow(z, x), value_flow(z, y).
        value_alias(x, y) :- value_flow(z, x), memory_alias(z, w), value_flow(w, y).

        value_flow(x, x) :- assign(x, y).
        value_flow(x, x) :- assign(y, x).

        memory_alias(x, x) :- assign(y, x).
        memory_alias(x, x) :- assign(x, y).
    "#;

    let output = generate_skeleton_datafrog(text);

    let expected = r#"
// Extensional predicates, and their indices

let assign: Relation<(u32, u32)> = Vec::new().into();

// Note: `assign_b` is an indexed version of the input facts `assign`
let assign_b: Relation<(u32, u32)> = Vec::new().into();

// Note: `dereference_a` is an indexed version of the input facts `dereference`
let dereference_a: Relation<(u32, u32)> = Vec::new().into();

// Note: couldn't infer output relation automatically

let mut iteration = Iteration::new();

// Intensional predicates, and their indices

let memory_alias = iteration.variable::<(u32, u32)>("memory_alias");

// Note: `memory_alias_a` is an indexed version of the `memory_alias` relation
let memory_alias_a = iteration.variable::<(u32, u32)>("memory_alias_a");
let memory_alias_step_4_1 = iteration.variable("memory_alias_step_4_1");
let value_alias = iteration.variable::<(u32, u32)>("value_alias");

// Note: `value_alias_a` is an indexed version of the `value_alias` relation
let value_alias_a = iteration.variable::<(u32, u32)>("value_alias_a");
let value_alias_step_6_1 = iteration.variable("value_alias_step_6_1");
let value_flow = iteration.variable::<(u32, u32)>("value_flow");

// Note: `value_flow_a` is an indexed version of the `value_flow` relation
let value_flow_a = iteration.variable::<(u32, u32)>("value_flow_a");

// Note: `value_flow_b` is an indexed version of the `value_flow` relation
let value_flow_b = iteration.variable::<(u32, u32)>("value_flow_b");

// R01: value_flow(y, x) :- assign(y, x).
value_flow.extend(assign.iter().clone());

// R07: value_flow(x, x) :- assign(x, y).
value_flow.extend(assign.iter().map(|&(x, _y)| (x, x)));

// R08: value_flow(x, x) :- assign(y, x).
value_flow.extend(assign.iter().map(|&(_y, x)| (x, x)));

// R09: memory_alias(x, x) :- assign(y, x).
memory_alias.extend(assign.iter().map(|&(_y, x)| (x, x)));

// R10: memory_alias(x, x) :- assign(x, y).
memory_alias.extend(assign.iter().map(|&(x, _y)| (x, x)));

while iteration.changed() {

    // Index maintenance
    value_flow_b.from_map(&value_flow, |&(_a, _b)| (z, x));
    value_flow_a.from_map(&value_flow, |&(_a, _b)| (w, y));
    value_alias_a.from_map(&value_alias, |&(_a, _b)| (y, z));
    memory_alias_a.from_map(&memory_alias, |&(_a, _b)| (z, w));

    // Rules

    // R01: value_flow(y, x) :- assign(y, x).
    // `assign` is a static input, already loaded into `value_flow`.

    // R02: value_flow(x, y) :- memory_alias(z, y), assign(x, z).
    value_flow.from_join(&memory_alias_a, &assign_b, |&_z, &y, &x| (x, y));

    // R03: value_flow(x, y) :- value_flow(x, z), value_flow(z, y).
    value_flow.from_join(&value_flow_b, &value_flow_a, |&_z, &x, &y| (x, y));

    // R04: memory_alias(x, w) :- value_alias(y, z), dereference(y, x), dereference(z, w).
    memory_alias_step_4_1.from_join(&value_alias_a, &dereference_a, |&_y, &z, &x| (z, x));
    memory_alias.from_join(&memory_alias_step_4_1, &dereference_a, |&_z, &x, &w| (x, w));

    // R05: value_alias(x, y) :- value_flow(z, x), value_flow(z, y).
    value_alias.from_join(&value_flow_a, &value_flow_a, |&_z, &x, &y| (x, y));

    // R06: value_alias(x, y) :- value_flow(z, x), memory_alias(z, w), value_flow(w, y).
    value_alias_step_6_1.from_join(&value_flow_a, &memory_alias_a, |&_z, &x, &w| (w, x));
    value_alias.from_join(&value_alias_step_6_1, &value_flow_a, |&_w, &x, &y| (x, y));

    // R07: value_flow(x, x) :- assign(x, y).
    // `assign` is a static input, already loaded into `value_flow`.

    // R08: value_flow(x, x) :- assign(y, x).
    // `assign` is a static input, already loaded into `value_flow`.

    // R09: memory_alias(x, x) :- assign(y, x).
    // `assign` is a static input, already loaded into `memory_alias`.

    // R10: memory_alias(x, x) :- assign(x, y).
    // `assign` is a static input, already loaded into `memory_alias`.
}
"#;
    println!("{}", output);
    assert_eq!(expected, output);
}

#[allow(dead_code, unused_variables)]
fn ensure_generated_rules_build() {
    // shim to bring in datafrog so that the generated skeleton can build.
    use datafrog::{Iteration, Relation};

    // ----- output from the skeleton generator follows below (+ manual comments) -----

    // Extensional predicates, and their indices

    let assign: Relation<(u32, u32)> = Vec::new().into();

    // Note: `assign_b` is an indexed version of the input facts `assign`
    let assign_b: Relation<(u32, u32)> = Vec::new().into();

    // Note: `dereference_a` is an indexed version of the input facts `dereference`
    let dereference_a: Relation<(u32, u32)> = Vec::new().into();

    // Note: couldn't infer output relation automatically

    let mut iteration = Iteration::new();

    // Intensional predicates, and their indices

    let memory_alias = iteration.variable::<(u32, u32)>("memory_alias");

    // Note: `memory_alias_a` is an indexed version of the `memory_alias` relation
    let memory_alias_a = iteration.variable::<(u32, u32)>("memory_alias_a");
    let memory_alias_step_4_1 = iteration.variable("memory_alias_step_4_1");
    let value_alias = iteration.variable::<(u32, u32)>("value_alias");

    // Note: `value_alias_a` is an indexed version of the `value_alias` relation
    let value_alias_a = iteration.variable::<(u32, u32)>("value_alias_a");
    let value_alias_step_6_1 = iteration.variable("value_alias_step_6_1");
    let value_flow = iteration.variable::<(u32, u32)>("value_flow");

    // Note: `value_flow_a` is an indexed version of the `value_flow` relation
    let value_flow_a = iteration.variable::<(u32, u32)>("value_flow_a");

    // Note: `value_flow_b` is an indexed version of the `value_flow` relation
    let value_flow_b = iteration.variable::<(u32, u32)>("value_flow_b");

    // R01: value_flow(y, x) :- assign(y, x).
    value_flow.extend(assign.iter().clone());

    // R07: value_flow(x, x) :- assign(x, y).
    value_flow.extend(assign.iter().map(|&(x, _y)| (x, x)));

    // R08: value_flow(x, x) :- assign(y, x).
    value_flow.extend(assign.iter().map(|&(_y, x)| (x, x)));

    // R09: memory_alias(x, x) :- assign(y, x).
    memory_alias.extend(assign.iter().map(|&(_y, x)| (x, x)));

    // R10: memory_alias(x, x) :- assign(x, y).
    memory_alias.extend(assign.iter().map(|&(x, _y)| (x, x)));

    while iteration.changed() {

        // Index maintenance

        // The generator produces, as of now, this piece of code for index maintenance:
        //
        //     value_flow_b.from_map(&value_flow, |&(_a, _b)| (z, x));
        //     value_flow_a.from_map(&value_flow, |&(_a, _b)| (w, y));
        //     value_alias_a.from_map(&value_alias, |&(_a, _b)| (y, z));
        //     memory_alias_a.from_map(&memory_alias, |&(_a, _b)| (z, w));
        //
        // which is
        // - invalid, as it references the non-canonicalized names in the produced tuple
        // - inefficient, as the non-canonicalized arguments were recorded as uses of more indexes
        //   than needed: all these are indexed on the first column, and that is already the case in
        //   the original relation.

        // I've manually changed the naming below until index maintenance is fixed for this
        // case where the projections use completely different names from the parameter declarations.
        value_flow_b.from_map(&value_flow, |&(a, b)| (b, a));
        value_flow_a.from_map(&value_flow, |&(a, b)| (a, b)); // useless index
        value_alias_a.from_map(&value_alias, |&(a, b)| (a, b)); // useless index
        memory_alias_a.from_map(&memory_alias, |&(a, b)| (a, b)); // useless index

        // Rules

        // R01: value_flow(y, x) :- assign(y, x).
        // `assign` is a static input, already loaded into `value_flow`.

        // R02: value_flow(x, y) :- memory_alias(z, y), assign(x, z).
        value_flow.from_join(&memory_alias_a, &assign_b, |&_z, &y, &x| (x, y));

        // R03: value_flow(x, y) :- value_flow(x, z), value_flow(z, y).
        value_flow.from_join(&value_flow_b, &value_flow_a, |&_z, &x, &y| (x, y));

        // R04: memory_alias(x, w) :- value_alias(y, z), dereference(y, x), dereference(z, w).
        memory_alias_step_4_1.from_join(&value_alias_a, &dereference_a, |&_y, &z, &x| (z, x));
        memory_alias.from_join(&memory_alias_step_4_1, &dereference_a, |&_z, &x, &w| (x, w));

        // R05: value_alias(x, y) :- value_flow(z, x), value_flow(z, y).
        value_alias.from_join(&value_flow_a, &value_flow_a, |&_z, &x, &y| (x, y));

        // R06: value_alias(x, y) :- value_flow(z, x), memory_alias(z, w), value_flow(w, y).
        value_alias_step_6_1.from_join(&value_flow_a, &memory_alias_a, |&_z, &x, &w| (w, x));
        value_alias.from_join(&value_alias_step_6_1, &value_flow_a, |&_w, &x, &y| (x, y));

        // R07: value_flow(x, x) :- assign(x, y).
        // `assign` is a static input, already loaded into `value_flow`.

        // R08: value_flow(x, x) :- assign(y, x).
        // `assign` is a static input, already loaded into `value_flow`.

        // R09: memory_alias(x, x) :- assign(y, x).
        // `assign` is a static input, already loaded into `memory_alias`.

        // R10: memory_alias(x, x) :- assign(x, y).
        // `assign` is a static input, already loaded into `memory_alias`.
    }
}
