use datapond::generate_skeleton_datafrog;
use pretty_assertions::assert_eq;

#[test]
fn generate_galen_rules() {
    // Adapted from https://github.com/frankmcsherry/blog/blob/master/posts/2025-08-02.md

    let text = r#"
        input c(a: u32, b: u32, c: u32)
        input input_p(x: u32, y: u32)
        input input_q(a: u32, b: u32, c: u32)
        input r(a: u32, b: u32, c: u32)
        input s(x: u32, y: u32)
        input u(a: u32, b: u32, c: u32)

        output p(x: u32, y: u32)
        output q(a: u32, b: u32, c: u32)

        p(x, z) :- p(x, y), p(y, z).
        p(x, z) :- p(y, w), u(w, r, z), q(x, r, y).
        p(x, z) :- c(y, w, z), p(x, w), p(x, y).
        q(x, r, z) :- p(x, y), q(y, r, z).
        q(x, q, z) :- q(x, r, z), s(r, q).
        q(x, e, o) :- q(x, y, z), r(y, u, e), q(z, u, o).

        p(x, y) :- input_p(x, y).
        q(a, b, c) :- input_q(a, b, c).
    "#;

    let output = generate_skeleton_datafrog(text, true);

    let expected = r#"
// Extensional predicates, and their indices

let input_p: datafrog::Relation<(u32, u32)> = input_p.into();
let input_q: datafrog::Relation<(u32, u32, u32)> = input_q.into();

// Note: `r_a` is an indexed version of the input facts `r`
let r_a: datafrog::Relation<(u32, (u32, u32))> = datafrog::Relation::from_iter(
    r.iter().map(|&(a, b, c)| (a, (b, c)))
);

// Note: `s_x` is an indexed version of the input facts `s`
let s_x: datafrog::Relation<(u32, u32)> = datafrog::Relation::from_iter(
    s.iter().map(|&(x, y)| (x, y))
);

// Note: `u_a` is an indexed version of the input facts `u`
let u_a: datafrog::Relation<(u32, (u32, u32))> = datafrog::Relation::from_iter(
    u.iter().map(|&(a, b, c)| (a, (b, c)))
);

// Note: `c_b` is an indexed version of the input facts `c`
let c_b: datafrog::Relation<(u32, (u32, u32))> = datafrog::Relation::from_iter(
    c.iter().map(|&(a, b, c)| (b, (a, c)))
);

let (p, q) = {

let mut iteration = datafrog::Iteration::new();

// Intensional predicates, and their indices

let p = iteration.variable::<((u32, u32), ())>("p");
let p_step_2_1 = iteration.variable("p_step_2_1");
let p_step_3_1 = iteration.variable("p_step_3_1");

// Note: `p_x` is an indexed version of the `p` relation
let p_x = iteration.variable_indistinct::<(u32, u32)>("p_x");

// Note: `p_y` is an indexed version of the `p` relation
let p_y = iteration.variable_indistinct::<(u32, u32)>("p_y");
let q = iteration.variable::<(u32, u32, u32)>("q");

// Note: `q_a` is an indexed version of the `q` relation
let q_a = iteration.variable_indistinct::<(u32, (u32, u32))>("q_a");

// Note: `q_ab` is an indexed version of the `q` relation
let q_ab = iteration.variable_indistinct::<((u32, u32), u32)>("q_ab");

// Note: `q_b` is an indexed version of the `q` relation
let q_b = iteration.variable_indistinct::<(u32, (u32, u32))>("q_b");

// Note: `q_bc` is an indexed version of the `q` relation
let q_bc = iteration.variable_indistinct::<((u32, u32), u32)>("q_bc");
let q_step_6_1 = iteration.variable("q_step_6_1");

// Duplicate extensional predicates used as `Variable`s

let c_b = {
    let var = iteration.variable_indistinct("c_b");
    var.extend(c_b.iter().clone());
    var
};

// R07: p(x, y) :- input_p(x, y).
p.extend(input_p.iter().map(|&tuple| (tuple, ())));

// R08: q(a, b, c) :- input_q(a, b, c).
q.extend(input_q.iter().clone());

while iteration.changed() {

    // Index maintenance
    q_b.from_map(&q, |&(a, b, c)| (b, (a, c)));
    q_bc.from_map(&q, |&(a, b, c)| ((b, c), a));
    q_a.from_map(&q, |&(a, b, c)| (a, (b, c)));
    q_ab.from_map(&q, |&(a, b, c)| ((a, b), c));
    p_x.from_map(&p, |&((x, y), _)| (x, y));
    p_y.from_map(&p, |&((x, y), _)| (y, x));

    // Rules

    // R01: p(x, z) :- p(x, y), p(y, z).
    p.from_join(&p_y, &p_x, |&_y, &x, &z| ((x, z), ()));

    // R02: p(x, z) :- p(y, w), u(w, r, z), q(x, r, y).
    p_step_2_1.from_join(&p_y, &u_a, |&_w, &y, &(r, z)| ((r, y), z));
    p.from_join(&p_step_2_1, &q_bc, |&(_r, _y), &z, &x| ((x, z), ()));

    // R03: p(x, z) :- c(y, w, z), p(x, w), p(x, y).
    p_step_3_1.from_join(&c_b, &p_y, |&_w, &(y, z), &x| ((x, y), z));
    p.from_join(&p_step_3_1, &p, |&(x, _y), &z, _| ((x, z), ()));

    // R04: q(x, r, z) :- p(x, y), q(y, r, z).
    q.from_join(&p_y, &q_a, |&_y, &x, &(r, z)| (x, r, z));

    // R05: q(x, q, z) :- q(x, r, z), s(r, q).
    q.from_join(&q_b, &s_x, |&_r, &(x, z), &q| (x, q, z));

    // R06: q(x, e, o) :- q(x, y, z), r(y, u, e), q(z, u, o).
    q_step_6_1.from_join(&q_b, &r_a, |&_y, &(x, z), &(u, e)| ((z, u), (x, e)));
    q.from_join(&q_step_6_1, &q_ab, |&(_z, _u), &(x, e), &o| (x, e, o));

    // R07: p(x, y) :- input_p(x, y).
    // `input_p` is a static input, already loaded into `p`.

    // R08: q(a, b, c) :- input_q(a, b, c).
    // `input_q` is a static input, already loaded into `q`.
}

// The computation results: the completed output relations from the program
(p.complete(), q.complete())
};
"#;
    println!("{}", output);
    assert_eq!(expected, output);
}

#[allow(dead_code, unused_variables)]
fn ensure_generated_rules_build() {
    // shim to simulate inputs so that the generated skeleton can build.
    let c: Vec<(u32, u32, u32)> = Vec::new();
    let input_p: Vec<(u32, u32)> = Vec::new();
    let input_q: Vec<(u32, u32, u32)> = Vec::new();
    let r: Vec<(u32, u32, u32)> = Vec::new();
    let s: Vec<(u32, u32)> = Vec::new();
    let u: Vec<(u32, u32, u32)> = Vec::new();

    // ----- output from the skeleton generator follows below (+ manual comments) -----

    // Extensional predicates, and their indices

    let input_p: datafrog::Relation<(u32, u32)> = input_p.into();
    let input_q: datafrog::Relation<(u32, u32, u32)> = input_q.into();

    // Note: `r_a` is an indexed version of the input facts `r`
    let r_a: datafrog::Relation<(u32, (u32, u32))> =
        datafrog::Relation::from_iter(r.iter().map(|&(a, b, c)| (a, (b, c))));

    // Note: `s_x` is an indexed version of the input facts `s` // useless index
    let s_x: datafrog::Relation<(u32, u32)> =
        datafrog::Relation::from_iter(s.iter().map(|&(x, y)| (x, y)));

    // Note: `u_a` is an indexed version of the input facts `u`
    let u_a: datafrog::Relation<(u32, (u32, u32))> =
        datafrog::Relation::from_iter(u.iter().map(|&(a, b, c)| (a, (b, c))));

    // Note: `c_b` is an indexed version of the input facts `c`
    let c_b: datafrog::Relation<(u32, (u32, u32))> =
        datafrog::Relation::from_iter(c.iter().map(|&(a, b, c)| (b, (a, c))));

    let (p, q) = {
        let mut iteration = datafrog::Iteration::new();

        // Intensional predicates, and their indices

        let p = iteration.variable::<((u32, u32), ())>("p");
        let p_step_2_1 = iteration.variable("p_step_2_1");
        let p_step_3_1 = iteration.variable("p_step_3_1");

        // Note: `p_x` is an indexed version of the `p` relation
        let p_x = iteration.variable_indistinct::<(u32, u32)>("p_x");

        // Note: `p_y` is an indexed version of the `p` relation
        let p_y = iteration.variable_indistinct::<(u32, u32)>("p_y");
        let q = iteration.variable::<(u32, u32, u32)>("q");

        // Note: `q_a` is an indexed version of the `q` relation
        let q_a = iteration.variable_indistinct::<(u32, (u32, u32))>("q_a");

        // Note: `q_ab` is an indexed version of the `q` relation
        let q_ab = iteration.variable_indistinct::<((u32, u32), u32)>("q_ab");

        // Note: `q_b` is an indexed version of the `q` relation
        let q_b = iteration.variable_indistinct::<(u32, (u32, u32))>("q_b");

        // Note: `q_bc` is an indexed version of the `q` relation
        let q_bc = iteration.variable_indistinct::<((u32, u32), u32)>("q_bc");
        let q_step_6_1 = iteration.variable("q_step_6_1");

        // Duplicate extensional predicates used as `Variable`s

        let c_b = {
            let var = iteration.variable_indistinct("c_b");
            var.extend(c_b.iter().clone());
            var
        };

        // R07: p(x, y) :- input_p(x, y).
        p.extend(input_p.iter().map(|&tuple| (tuple, ())));

        // R08: q(a, b, c) :- input_q(a, b, c).
        q.extend(input_q.iter().clone());

        while iteration.changed() {
            // Index maintenance
            q_b.from_map(&q, |&(a, b, c)| (b, (a, c)));
            q_bc.from_map(&q, |&(a, b, c)| ((b, c), a));
            q_a.from_map(&q, |&(a, b, c)| (a, (b, c)));
            q_ab.from_map(&q, |&(a, b, c)| ((a, b), c));
            p_x.from_map(&p, |&((x, y), _)| (x, y));
            p_y.from_map(&p, |&((x, y), _)| (y, x));

            // Rules

            // R01: p(x, z) :- p(x, y), p(y, z).
            p.from_join(&p_y, &p_x, |&_y, &x, &z| ((x, z), ()));

            // R02: p(x, z) :- p(y, w), u(w, r, z), q(x, r, y).
            p_step_2_1.from_join(&p_y, &u_a, |&_w, &y, &(r, z)| ((r, y), z));
            p.from_join(&p_step_2_1, &q_bc, |&(_r, _y), &z, &x| ((x, z), ()));

            // R03: p(x, z) :- c(y, w, z), p(x, w), p(x, y).
            p_step_3_1.from_join(&c_b, &p_y, |&_w, &(y, z), &x| ((x, y), z));
            p.from_join(&p_step_3_1, &p, |&(x, _y), &z, _| ((x, z), ()));

            // R04: q(x, r, z) :- p(x, y), q(y, r, z).
            q.from_join(&p_y, &q_a, |&_y, &x, &(r, z)| (x, r, z));

            // R05: q(x, q, z) :- q(x, r, z), s(r, q).
            q.from_join(&q_b, &s_x, |&_r, &(x, z), &q| (x, q, z));

            // R06: q(x, e, o) :- q(x, y, z), r(y, u, e), q(z, u, o).
            q_step_6_1.from_join(&q_b, &r_a, |&_y, &(x, z), &(u, e)| ((z, u), (x, e)));
            q.from_join(&q_step_6_1, &q_ab, |&(_z, _u), &(x, e), &o| (x, e, o));

            // R07: p(x, y) :- input_p(x, y).
            // `input_p` is a static input, already loaded into `p`.

            // R08: q(a, b, c) :- input_q(a, b, c).
            // `input_q` is a static input, already loaded into `q`.
        }

        // The computation results: the completed output relations from the program
        (p.complete(), q.complete())
    };
}
