### Datapond

A set of tools for working with [datafrog](https://github.com/rust-lang/datafrog) (until they are ready to be upstreamed there).

All these are made to be used in the limited context of [Polonius](https://github.com/rust-lang/polonius): not in general for all datalog computations. (Not to mention, datafrog itself has its peculiarities, and API requirements)

Currently containing:

1) a datalog-to-datafrog prototyping generator
2) some bugs

In the future, possibly:
- provenance information for generated tuples, and tracing provenance for a target tuple
- some kind of rule transformations (probably Demand Transformation)

---

### Datalog-to-datafrog prototyping generator

For _prototyping_ purposes, this will help generate a skeleton of a datafrog computation, from a set of Soufflé-like declarations, and _valid_ datalog rules (there is basically no error checking in the parser yet).

The generated skeleton should (hopefully) build as-is, but won't know where to get the data required by the relations. So it can help bootstrap using datafrog, and maybe get 80-90% of the way there.

In the Polonius context, this was used to bootstrap the `Naive` rules variant (the simpler variant) successfully just by filling the relations with data.


Using a set of declarations like these:

```prolog
input borrow_region(O: Origin, L: Loan, P: Point)
input cfg_edge(P: Point, Q: Point)
input killed(L: Loan, P: Point)
input outlives(O1: Origin, O2: Origin, P: Point)
input region_live_at(O: Origin, P: Point)
internal subset(O1: Origin, O2: Origin, P: Point)
internal requires(O: Origin, L: Loan, P: Point)
internal borrow_live_at(L: Loan, P: Point)
input invalidates(L: Loan, P: Point)
output errors(L: Loan, P: Point)
```

And the `Naive` polonius rules:

```prolog
// R1
subset(O1, O2, P) :- 
  outlives(O1, O2, P).

// R2
subset(O1, O3, P) :- 
  subset(O1, O2, P), 
  subset(O2, O3, P).

// R3
subset(O1, O2, Q) :- 
  subset(O1, O2, P), 
  cfg_edge(P, Q), 
  region_live_at(O1, Q), 
  region_live_at(O2, Q).

// R4
requires(O, L, P) :- 
  borrow_region(O, L, P).

// R5
requires(O2, L, P) :- 
  requires(O1, L, P),
  subset(O1, O2, P).

// R6
requires(O, L, Q) :- 
  requires(O, L, P), 
  !killed(L, P), 
  cfg_edge(P, Q), 
  region_live_at(O, Q).

// R7
borrow_live_at(L, P) :- 
  requires(O, L, P), 
  region_live_at(O, P).

// R8
errors(L, P) :- 
  borrow_live_at(L, P), 
  invalidates(L, P).
```

The `datapond::generate_skeleton_datafrog` function will generate this piece of code (`rustfmt`-ed here)
<details>

```rust
// Extensional predicates, and their indices

let borrow_region: Relation<(Origin, Loan, Point)> = Vec::new().into();

// Note: `cfg_edge_p` is an indexed version of the input facts `cfg_edge`
let cfg_edge_p: Relation<(Point, Point)> = Vec::new().into();

let invalidates: Relation<((Loan, Point), ())> = Vec::new().into();
let killed: Relation<(Loan, Point)> = Vec::new().into();
let outlives: Relation<(Origin, Origin, Point)> = Vec::new().into();
let region_live_at: Relation<((Origin, Point), ())> = Vec::new().into();

// `errors` inferred as the output relation
let errors = {
    let mut iteration = Iteration::new();
    // Intensional predicates, and their indices

    let borrow_live_at = iteration.variable::<((Loan, Point), ())>("borrow_live_at");
    let errors = iteration.variable::<(Loan, Point)>("errors");
    let requires = iteration.variable::<(Origin, Loan, Point)>("requires");

    // Note: `requires_lp` is an indexed version of the `requires` relation
    let requires_lp = iteration.variable::<((Loan, Point), Origin)>("requires_lp");

    // Note: `requires_op` is an indexed version of the `requires` relation
    let requires_op = iteration.variable::<((Origin, Point), Loan)>("requires_op");
    let requires_step_6_1 = iteration.variable("requires_step_6_1");
    let requires_step_6_2 = iteration.variable("requires_step_6_2");
    let subset = iteration.variable::<(Origin, Origin, Point)>("subset");

    // Note: `subset_o1p` is an indexed version of the `subset` relation
    let subset_o1p = iteration.variable::<((Origin, Point), Origin)>("subset_o1p");

    // Note: `subset_o2p` is an indexed version of the `subset` relation
    let subset_o2p = iteration.variable::<((Origin, Point), Origin)>("subset_o2p");

    // Note: `subset_p` is an indexed version of the `subset` relation
    let subset_p = iteration.variable::<(Point, (Origin, Origin))>("subset_p");
    let subset_step_3_1 = iteration.variable("subset_step_3_1");
    let subset_step_3_2 = iteration.variable("subset_step_3_2");

    // R01: subset(O1, O2, P) :- outlives(O1, O2, P).
    subset.extend(outlives.iter().clone());

    // R04: requires(O, L, P) :- borrow_region(O, L, P).
    requires.extend(borrow_region.iter().map(|&tuple| tuple));

    while iteration.changed() {
        // Index maintenance
        requires_op.from_map(&requires, |&(o, l, p)| ((o, p), l));
        requires_lp.from_map(&requires, |&(o, l, p)| ((l, p), o));
        subset_o2p.from_map(&subset, |&(o1, o2, p)| ((o2, p), o1));
        subset_o1p.from_map(&subset, |&(o1, o2, p)| ((o1, p), o2));
        subset_p.from_map(&subset, |&(o1, o2, p)| (p, (o1, o2)));

        // Rules

        // R01: subset(O1, O2, P) :- outlives(O1, O2, P).
        // `outlives` is a static input, already loaded into `subset`.

        // R02: subset(O1, O3, P) :- subset(O1, O2, P), subset(O2, O3, P).
        subset.from_join(&subset_o2p, &subset_o1p, |&(_o2, p), &o1, &o3| (o1, o3, p));

        // R03: subset(O1, O2, Q) :- subset(O1, O2, P), cfg_edge(P, Q), region_live_at(O1, Q), region_live_at(O2, Q).
        subset_step_3_1.from_join(&subset_p, &cfg_edge_p, |&_p, &(o1, o2), &q| ((o1, q), o2));
        subset_step_3_2.from_join(&subset_step_3_1, &region_live_at, |&(o1, q), &o2, _| {
            ((o2, q), o1)
        });
        subset.from_join(&subset_step_3_2, &region_live_at, |&(o2, q), &o1, _| {
            (o1, o2, q)
        });

        // R04: requires(O, L, P) :- borrow_region(O, L, P).
        // `borrow_region` is a static input, already loaded into `requires`.

        // R05: requires(O2, L, P) :- requires(O1, L, P), subset(O1, O2, P).
        requires.from_join(&requires_op, &subset_o1p, |&(_o1, p), &l, &o2| (o2, l, p));

        // R06: requires(O, L, Q) :- requires(O, L, P), !killed(L, P), cfg_edge(P, Q), region_live_at(O, Q).
        requires_step_6_1.from_antijoin(&requires_lp, &killed, |&(l, p), &o| (p, (l, o)));
        requires_step_6_2.from_join(&requires_step_6_1, &cfg_edge_p, |&_p, &(l, o), &q| {
            ((o, q), l)
        });
        requires.from_join(&requires_step_6_2, &region_live_at, |&(o, q), &l, _| {
            (o, l, q)
        });

        // R07: borrow_live_at(L, P) :- requires(O, L, P), region_live_at(O, P).
        borrow_live_at.from_join(&requires_op, &region_live_at, |&(_o, p), &l, _| {
            ((l, p), ())
        });

        // R08: errors(L, P) :- borrow_live_at(L, P), invalidates(L, P).
        errors.from_join(&borrow_live_at, &invalidates, |&(l, p), _, _| (l, p));
    }

    errors.complete()
};
```

</details>


More detailed examples, showing usage and output, can also be found in the [examples](./examples) and [tests](./tests) directories.

Currently not supported:
- purely extensional joins in rules (datafrog is mostly made for intensional predicate computations), maybe later but we're not using those right now.
- datafrog's API requires intensional predicates to be first in join steps, there should be a warning/error for datalog rules not following this pattern. The skeleton generator will generate a "correct join" (with respect to key and values tuples) but it will not compile. The 2 predicates can easily be swapped if that happens.
- populating extensional indices (unlike intensional indices).
- some kinds of self joins.
- leapjoins: those are mostly optimisations over regular joins. Since the purpose of this generator is to easily try different rules and transformations, they can be added later (or manually after generating the skeleton).