use datapond::generate_skeleton_datafrog;

#[test]
fn generate_flow_sensitive_equality_rules() {
    let decls = r#"
        .decl borrow_region(O: Origin, L: Loan, P: Point)
        .decl cfg_edge(P: Point, Q: Point)
        .decl killed(L: Loan, P: Point)
        .decl outlives(O1: Origin, O2: Origin, P: Point)
        .decl region_live_at(O: Origin, P: Point)
        .decl subset(O1: Origin, O2: Origin, P: Point)
        .decl requires(O: Origin, L: Loan, P: Point)
        .decl borrow_live_at(L: Loan, P: Point)
        .decl invalidates(L: Loan, P: Point)
        .decl errors(L: Loan, P: Point)
        .decl equals(O1: Origin, O2: Origin, P: Point)
    "#;

    let rules = r#"
        // R1
        subset(O1, O2, P) :-
          outlives(O1, O2, P).

        // R2
        subset(O1, O3, P) :- 
          subset(O1, O2, P), 
          outlives(O2, O3, P).
        
        // R3
        equals(O1, O2, P) :-
          subset(O1, O2, P), 
          subset(O2, O1, P).

        // R4
        equals(O1, O3, P) :-
          equals(O1, O2, P),
          equals(O2, O3, P).
        
        // R5
        equals(O1, O2, Q) :-
          equals(O1, O2, P),
          cfg_edge(P, Q),
          region_live_at(O1, Q),
          region_live_at(O2, Q).

        // R6
        requires(O2, L, P) :-
          requires(O1, L, P),
          equals(O1, O2, P).

        // R7
        requires(O, L, P) :- 
          borrow_region(O, L, P).

        // R8
        requires(O2, L, P) :- 
          requires(O1, L, P),
          subset(O1, O2, P).
    
        // R9
        requires(O, L, Q) :- 
          requires(O, L, P), 
          !killed(L, P), 
          cfg_edge(P, Q), 
          region_live_at(O, Q).

        // R10
        borrow_live_at(L, P) :- 
          requires(O, L, P), 
          region_live_at(O, P).

        // R11
        errors(L, P) :- 
          borrow_live_at(L, P),
          invalidates(L, P).
    "#;

    let mut output = String::new();
    generate_skeleton_datafrog(decls, rules, &mut output);

    let expected = r#"
// Extensional predicates, and their indices

let borrow_region: Relation<(Origin, Loan, Point)> = Vec::new().into();

// Note: `cfg_edge_p` is an indexed version of the input facts `cfg_edge`
let cfg_edge_p: Relation<(Point, Point)> = Vec::new().into();

let invalidates: Relation<((Loan, Point), ())> = Vec::new().into();
let killed: Relation<(Loan, Point)> = Vec::new().into();
let outlives: Relation<(Origin, Origin, Point)> = Vec::new().into();

// Note: `outlives_o1p` is an indexed version of the input facts `outlives`
let outlives_o1p: Relation<((Origin, Point), Origin)> = Vec::new().into();

let region_live_at: Relation<((Origin, Point), ())> = Vec::new().into();

// `errors` inferred as the output relation
let errors = {

let mut iteration = Iteration::new();

// Intensional predicates, and their indices

let borrow_live_at = iteration.variable::<((Loan, Point), ())>("borrow_live_at");
let equals = iteration.variable::<(Origin, Origin, Point)>("equals");

// Note: `equals_o1p` is an indexed version of the `equals` relation
let equals_o1p = iteration.variable::<((Origin, Point), Origin)>("equals_o1p");

// Note: `equals_o2p` is an indexed version of the `equals` relation
let equals_o2p = iteration.variable::<((Origin, Point), Origin)>("equals_o2p");

// Note: `equals_p` is an indexed version of the `equals` relation
let equals_p = iteration.variable::<(Point, (Origin, Origin))>("equals_p");
let equals_step_5_1 = iteration.variable("equals_step_5_1");
let equals_step_5_2 = iteration.variable("equals_step_5_2");
let errors = iteration.variable::<(Loan, Point)>("errors");
let requires = iteration.variable::<(Origin, Loan, Point)>("requires");

// Note: `requires_lp` is an indexed version of the `requires` relation
let requires_lp = iteration.variable::<((Loan, Point), Origin)>("requires_lp");

// Note: `requires_op` is an indexed version of the `requires` relation
let requires_op = iteration.variable::<((Origin, Point), Loan)>("requires_op");
let requires_step_9_1 = iteration.variable("requires_step_9_1");
let requires_step_9_2 = iteration.variable("requires_step_9_2");
let subset = iteration.variable::<((Origin, Origin, Point), ())>("subset");

// Note: `subset_o1p` is an indexed version of the `subset` relation
let subset_o1p = iteration.variable::<((Origin, Point), Origin)>("subset_o1p");

// Note: `subset_o2o1p` is an indexed version of the `subset` relation
let subset_o2o1p = iteration.variable::<(((Origin, Origin, Point)), ())>("subset_o2o1p");

// Note: `subset_o2p` is an indexed version of the `subset` relation
let subset_o2p = iteration.variable::<((Origin, Point), Origin)>("subset_o2p");

// R01: subset(O1, O2, P) :- outlives(O1, O2, P).
subset.extend(outlives.iter().map(|&tuple| (tuple, ())));

// R07: requires(O, L, P) :- borrow_region(O, L, P).
requires.extend(borrow_region.iter().clone());

while iteration.changed() {

    // Index maintenance
    equals_p.from_map(&equals, |&(o1, o2, p)| (p, (o1, o2)));
    equals_o1p.from_map(&equals, |&(o1, o2, p)| ((o1, p), o2));
    subset_o1p.from_map(&subset, |&((o1, o2, p), _)| ((o1, p), o2));
    equals_o2p.from_map(&equals, |&(o1, o2, p)| ((o2, p), o1));
    subset_o2o1p.from_map(&subset, |&((o1, o2, p), _)| ((o2, o1, p), ()));
    requires_lp.from_map(&requires, |&(o, l, p)| ((l, p), o));
    subset_o2p.from_map(&subset, |&((o1, o2, p), _)| ((o2, p), o1));
    requires_op.from_map(&requires, |&(o, l, p)| ((o, p), l));

    // Rules

    // R01: subset(O1, O2, P) :- outlives(O1, O2, P).
    // `outlives` is a static input, already loaded into `subset`.

    // R02: subset(O1, O3, P) :- subset(O1, O2, P), outlives(O2, O3, P).
    subset.from_join(&subset_o2p, &outlives_o1p, |&(_o2, p), &o1, &o3| ((o1, o3, p), ()));

    // R03: equals(O1, O2, P) :- subset(O1, O2, P), subset(O2, O1, P).
    equals.from_join(&subset, &subset_o2o1p, |&(o1, o2, p), _, _| (o1, o2, p));

    // R04: equals(O1, O3, P) :- equals(O1, O2, P), equals(O2, O3, P).
    equals.from_join(&equals_o2p, &equals_o1p, |&(_o2, p), &o1, &o3| (o1, o3, p));

    // R05: equals(O1, O2, Q) :- equals(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
    equals_step_5_1.from_join(&equals_p, &cfg_edge_p, |&_p, &(o1, o2), &q| ((o1, q), o2));
    equals_step_5_2.from_join(&equals_step_5_1, &region_live_at, |&(o1, q), &o2, _| ((o2, q), o1));
    equals.from_join(&equals_step_5_2, &region_live_at, |&(o2, q), &o1, _| (o1, o2, q));

    // R06: requires(O2, L, P) :- requires(O1, L, P), equals(O1, O2, P).
    requires.from_join(&requires_op, &equals_o1p, |&(_o1, p), &l, &o2| (o2, l, p));

    // R07: requires(O, L, P) :- borrow_region(O, L, P).
    // `borrow_region` is a static input, already loaded into `requires`.

    // R08: requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
    requires.from_join(&requires_op, &subset_o1p, |&(_o1, p), &l, &o2| (o2, l, p));

    // R09: requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
    requires_step_9_1.from_antijoin(&requires_lp, &killed, |&(l, p), &o| (p, (l, o)));
    requires_step_9_2.from_join(&requires_step_9_1, &cfg_edge_p, |&_p, &(l, o), &q| ((o, q), l));
    requires.from_join(&requires_step_9_2, &region_live_at, |&(o, q), &l, _| (o, l, q));

    // R10: borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
    borrow_live_at.from_join(&requires_op, &region_live_at, |&(_o, p), &l, _| ((l, p), ()));

    // R11: errors(L, P) :- borrow_live_at(L, P), invalidates(L, P).
    errors.from_join(&borrow_live_at, &invalidates, |&(l, p), _, _| (l, p));
}

errors.complete()
};
"#;
    println!("{}", output);
    assert_eq!(expected, output);
}

// Copied from the above output test, to ensure what is generated at least builds, even
// if it cannot run due to having no data. The generic types are to bring in the ones
// defined in the declarations.
// TODO: add a build and generate step, and include the result in this function
#[allow(dead_code, unused_variables)]
fn ensure_generated_rules_build<Origin, Loan, Point>()
where
    Origin: Ord + Copy + 'static,
    Loan: Ord + Copy + 'static,
    Point: Ord + Copy + 'static,
{
    // shim to bring in datafrog so that the generated skeleton can build.
    use datafrog::{Iteration, Relation};

    // ----- output from the skeleton generator follows below -----

    // Extensional predicates, and their indices

    let borrow_region: Relation<(Origin, Loan, Point)> = Vec::new().into();

    // Note: `cfg_edge_p` is an indexed version of the input facts `cfg_edge`
    let cfg_edge_p: Relation<(Point, Point)> = Vec::new().into();

    let invalidates: Relation<((Loan, Point), ())> = Vec::new().into();
    let killed: Relation<(Loan, Point)> = Vec::new().into();
    let outlives: Relation<(Origin, Origin, Point)> = Vec::new().into();

    // Note: `outlives_o1p` is an indexed version of the input facts `outlives`
    let outlives_o1p: Relation<((Origin, Point), Origin)> = Vec::new().into();

    let region_live_at: Relation<((Origin, Point), ())> = Vec::new().into();

    // `errors` inferred as the output relation
    let errors = {
        let mut iteration = Iteration::new();

        // Intensional predicates, and their indices

        let borrow_live_at = iteration.variable::<((Loan, Point), ())>("borrow_live_at");
        let equals = iteration.variable::<(Origin, Origin, Point)>("equals");

        // Note: `equals_o1p` is an indexed version of the `equals` relation
        let equals_o1p = iteration.variable::<((Origin, Point), Origin)>("equals_o1p");

        // Note: `equals_p` is an indexed version of the `equals` relation
        let equals_p = iteration.variable::<(Point, (Origin, Origin))>("equals_p");
        let errors = iteration.variable::<(Loan, Point)>("errors");
        let requires = iteration.variable::<(Origin, Loan, Point)>("requires");

        // Note: `requires_lp` is an indexed version of the `requires` relation
        let requires_lp = iteration.variable::<((Loan, Point), Origin)>("requires_lp");

        // Note: `requires_op` is an indexed version of the `requires` relation
        let requires_op = iteration.variable::<((Origin, Point), Loan)>("requires_op");
        let requires_step_8_1 = iteration.variable("requires_step_8_1");
        let requires_step_8_2 = iteration.variable("requires_step_8_2");
        let subset = iteration.variable::<((Origin, Origin, Point), ())>("subset");

        // Note: `subset_o1p` is an indexed version of the `subset` relation
        let subset_o1p = iteration.variable::<((Origin, Point), Origin)>("subset_o1p");

        // Note: `subset_o2o1p` is an indexed version of the `subset` relation
        let subset_o2o1p = iteration.variable::<(((Origin, Origin, Point)), ())>("subset_o2o1p");

        // Note: `subset_o2p` is an indexed version of the `subset` relation
        let subset_o2p = iteration.variable::<((Origin, Point), Origin)>("subset_o2p");

        // R01: subset(O1, O2, P) :- outlives(O1, O2, P).
        subset.extend(outlives.iter().map(|&tuple| (tuple, ())));

        // R06: requires(O, L, P) :- borrow_region(O, L, P).
        requires.extend(borrow_region.iter().clone());

        while iteration.changed() {
            // Index maintenance
            subset_o1p.from_map(&subset, |&((o1, o2, p), _)| ((o1, p), o2));
            equals_p.from_map(&equals, |&(o1, o2, p)| (p, (o1, o2)));
            requires_lp.from_map(&requires, |&(o, l, p)| ((l, p), o));
            subset_o2p.from_map(&subset, |&((o1, o2, p), _)| ((o2, p), o1));
            equals_o1p.from_map(&equals, |&(o1, o2, p)| ((o1, p), o2));
            requires_op.from_map(&requires, |&(o, l, p)| ((o, p), l));
            subset_o2o1p.from_map(&subset, |&((o1, o2, p), _)| ((o2, o1, p), ()));

            // Rules

            // R01: subset(O1, O2, P) :- outlives(O1, O2, P).
            // `outlives` is a static input, already loaded into `subset`.

            // R02: subset(O1, O3, P) :- subset(O1, O2, P), outlives(O2, O3, P).
            subset.from_join(&subset_o2p, &outlives_o1p, |&(_o2, p), &o1, &o3| {
                ((o1, o3, p), ())
            });

            // R03: equals(O1, O2, P) :- subset(O1, O2, P), subset(O2, O1, P).
            equals.from_join(&subset, &subset_o2o1p, |&(o1, o2, p), _, _| (o1, o2, p));

            // R04: equals(O1, O2, Q) :- equals(O1, O2, P), cfg_edge(P, Q).
            equals.from_join(&equals_p, &cfg_edge_p, |&_p, &(o1, o2), &q| (o1, o2, q));

            // R05: requires(O2, L, P) :- requires(O1, L, P), equals(O1, O2, P).
            requires.from_join(&requires_op, &equals_o1p, |&(_o1, p), &l, &o2| (o2, l, p));

            // R06: requires(O, L, P) :- borrow_region(O, L, P).
            // `borrow_region` is a static input, already loaded into `requires`.

            // R07: requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
            requires.from_join(&requires_op, &subset_o1p, |&(_o1, p), &l, &o2| (o2, l, p));

            // R08: requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
            requires_step_8_1.from_antijoin(&requires_lp, &killed, |&(l, p), &o| (p, (l, o)));
            requires_step_8_2.from_join(&requires_step_8_1, &cfg_edge_p, |&_p, &(l, o), &q| {
                ((o, q), l)
            });
            requires.from_join(&requires_step_8_2, &region_live_at, |&(o, q), &l, _| {
                (o, l, q)
            });

            // R09: borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
            borrow_live_at.from_join(&requires_op, &region_live_at, |&(_o, p), &l, _| {
                ((l, p), ())
            });

            // R10: errors(L, P) :- borrow_live_at(L, P), invalidates(L, P).
            errors.from_join(&borrow_live_at, &invalidates, |&(l, p), _, _| (l, p));
        }

        errors.complete()
    };
}
