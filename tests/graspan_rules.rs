use datapond::generate_skeleton_datafrog;
use pretty_assertions::assert_eq;

#[test]
fn generate_graspan_rules() {
    // Context-sensitive Points-to Analysis rules.
    // adapted from https://github.com/frankmcsherry/blog/blob/master/posts/2025-06-03.md#aliasing-analysis
    //
    // These were also adapted as the CSPA rules (and are in their own dedicated test), but slightly
    // differently.

    let text = r#"
        input minus_a(val: u32, loc: u32)
        input d(val: u32, loc: u32)

        output mfd(val: u32, loc: u32)
        output minus_m(val: u32, loc: u32)
        output fd(val: u32, loc: u32)

        mfd(l1, l2) :- minus_m(l3, l1), fd(l3, l2).
        minus_m(l2, l1) :- fd(l3, l1), fd(l3, l2).
        minus_m(l2, l1) :- fd(l3, l1), mfd(l3, l2).
        fd(val, unk) :- minus_a(loc, val), fd(loc, unk).
        fd(val, unk) :- minus_a(loc, val), mfd(loc, unk).
        fd(val, loc) :- d(val, loc).
    "#;

    let output = generate_skeleton_datafrog(text);

    let expected = r#"
// Extensional predicates, and their indices

let d: datafrog::Relation<(u32, u32)> = Vec::new().into();
let minus_a: datafrog::Relation<(u32, u32)> = Vec::new().into();

let (mfd, minus_m, fd) = {

let mut iteration = datafrog::Iteration::new();

// Intensional predicates, and their indices

let fd = iteration.variable::<(u32, u32)>("fd");
let mfd = iteration.variable::<(u32, u32)>("mfd");
let minus_m = iteration.variable::<(u32, u32)>("minus_m");

// Duplicate extensional predicates used as `Variable`s

let minus_a = {
    let var = iteration.variable::<(u32, u32)>("minus_a");
    var.extend(minus_a.iter().clone());
    var
};

// R06: fd(val, loc) :- d(val, loc).
fd.extend(d.iter().clone());

while iteration.changed() {
    // R01: mfd(l1, l2) :- minus_m(l3, l1), fd(l3, l2).
    mfd.from_join(&minus_m, &fd, |&_l3, &l1, &l2| (l1, l2));

    // R02: minus_m(l2, l1) :- fd(l3, l1), fd(l3, l2).
    minus_m.from_join(&fd, &fd, |&_l3, &l1, &l2| (l2, l1));

    // R03: minus_m(l2, l1) :- fd(l3, l1), mfd(l3, l2).
    minus_m.from_join(&fd, &mfd, |&_l3, &l1, &l2| (l2, l1));

    // R04: fd(val, unk) :- minus_a(loc, val), fd(loc, unk).
    fd.from_join(&minus_a, &fd, |&_loc, &val, &unk| (val, unk));

    // R05: fd(val, unk) :- minus_a(loc, val), mfd(loc, unk).
    fd.from_join(&minus_a, &mfd, |&_loc, &val, &unk| (val, unk));

    // R06: fd(val, loc) :- d(val, loc).
    // `d` is a static input, already loaded into `fd`.
}

// The computation results: the completed output relations from the program
(mfd.complete(), minus_m.complete(), fd.complete())
};
"#;
    println!("{}", output);
    assert_eq!(expected, output);
}

#[allow(dead_code, unused_variables)]
fn ensure_generated_rules_build() {
    // shim to bring in datafrog so that the generated skeleton can build.

    // ----- output from the skeleton generator follows below (+ manual comments) -----

    // Extensional predicates, and their indices

    let d: datafrog::Relation<(u32, u32)> = Vec::new().into();
    let minus_a: datafrog::Relation<(u32, u32)> = Vec::new().into();

    let (mfd, minus_m, fd) = {
        let mut iteration = datafrog::Iteration::new();

        // Intensional predicates, and their indices

        let fd = iteration.variable::<(u32, u32)>("fd");
        let mfd = iteration.variable::<(u32, u32)>("mfd");
        let minus_m = iteration.variable::<(u32, u32)>("minus_m");

        // Duplicate extensional predicates used as `Variable`s

        let minus_a = {
            let var = iteration.variable::<(u32, u32)>("minus_a");
            var.extend(minus_a.iter().clone());
            var
        };

        // R06: fd(val, loc) :- d(val, loc).
        fd.extend(d.iter().clone());

        while iteration.changed() {
            // R01: mfd(l1, l2) :- minus_m(l3, l1), fd(l3, l2).
            mfd.from_join(&minus_m, &fd, |&_l3, &l1, &l2| (l1, l2));

            // R02: minus_m(l2, l1) :- fd(l3, l1), fd(l3, l2).
            minus_m.from_join(&fd, &fd, |&_l3, &l1, &l2| (l2, l1));

            // R03: minus_m(l2, l1) :- fd(l3, l1), mfd(l3, l2).
            minus_m.from_join(&fd, &mfd, |&_l3, &l1, &l2| (l2, l1));

            // R04: fd(val, unk) :- minus_a(loc, val), fd(loc, unk).
            fd.from_join(&minus_a, &fd, |&_loc, &val, &unk| (val, unk));

            // R05: fd(val, unk) :- minus_a(loc, val), mfd(loc, unk).
            fd.from_join(&minus_a, &mfd, |&_loc, &val, &unk| (val, unk));

            // R06: fd(val, loc) :- d(val, loc).
            // `d` is a static input, already loaded into `fd`.
        }

        // The computation results: the completed output relations from the program
        (mfd.complete(), minus_m.complete(), fd.complete())
    };
}
