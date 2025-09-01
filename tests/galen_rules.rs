use datapond::generate_skeleton_datafrog;
use pretty_assertions::assert_eq;

#[test]
fn generate_galen_rules() {
    // Adapted from https://github.com/frankmcsherry/blog/blob/master/posts/2025-08-02.md

    let text = r#"
        input c(a: u32, b: u32, c: u32)
        input input_p(foo: u32, bar: u32)
        input input_q(m: u32, n: u32, o: u32)
        input r(d: u32, e: u32, f: u32)
        input s(x: u32, y: u32)
        input u(s: u32, t: u32, u: u32)

        output p(foo: u32, bar: u32)
        output q(m: u32, n: u32, o: u32)

        p(x, z) :- p(x, y), p(y, z).
        p(x, z) :- p(y, w), u(w, r, z), q(x, r, y).
        p(x, z) :- c(y, w, z), p(x, w), p(x, y).
        q(x, r, z) :- p(x, y), q(y, r, z).
        q(x, q, z) :- q(x, r, z), s(r, q).
        q(x, e, o) :- q(x, y, z), r(y, u, e), q(z, u, o).

        p(x, z) :- input_p(x, z).
        q(m, n, o) :- input_q(m, n, o).
    "#;

    let output = generate_skeleton_datafrog(text, true);

    let expected = r#"
// Extensional predicates, and their indices

let input_p: datafrog::Relation<(u32, u32)> = input_p.into();
let input_q: datafrog::Relation<(u32, u32, u32)> = input_q.into();

// Note: `r_d` is an indexed version of the input facts `r`
let r_d: datafrog::Relation<(u32, (u32, u32))> = Vec::new().into();

// Note: `s_x` is an indexed version of the input facts `s`
let s_x: datafrog::Relation<(u32, u32)> = Vec::new().into();

// Note: `u_s` is an indexed version of the input facts `u`
let u_s: datafrog::Relation<(u32, (u32, u32))> = Vec::new().into();

// Note: `c_b` is an indexed version of the input facts `c`
let c_b: datafrog::Relation<(u32, (u32, u32))> = Vec::new().into();

let (p, q) = {

let mut iteration = datafrog::Iteration::new();

// Intensional predicates, and their indices

let p = iteration.variable::<((u32, u32), ())>("p");

// Note: `p_bar` is an indexed version of the `p` relation
let p_bar = iteration.variable::<(u32, u32)>("p_bar");

// Note: `p_foo` is an indexed version of the `p` relation
let p_foo = iteration.variable::<(u32, u32)>("p_foo");
let p_step_2_1 = iteration.variable("p_step_2_1");
let p_step_3_1 = iteration.variable("p_step_3_1");
let q = iteration.variable::<(u32, u32, u32)>("q");

// Note: `q_m` is an indexed version of the `q` relation
let q_m = iteration.variable::<(u32, (u32, u32))>("q_m");

// Note: `q_mn` is an indexed version of the `q` relation
let q_mn = iteration.variable::<((u32, u32), u32)>("q_mn");

// Note: `q_n` is an indexed version of the `q` relation
let q_n = iteration.variable::<(u32, (u32, u32))>("q_n");

// Note: `q_no` is an indexed version of the `q` relation
let q_no = iteration.variable::<((u32, u32), u32)>("q_no");
let q_step_6_1 = iteration.variable("q_step_6_1");

// Duplicate extensional predicates used as `Variable`s

let c_b = {
    let var = iteration.variable("c_b");
    var.extend(c_b.iter().clone());
    var
};

// R07: p(x, z) :- input_p(x, z).
p.extend(input_p.iter().map(|&tuple| (tuple, ())));

// R08: q(m, n, o) :- input_q(m, n, o).
q.extend(input_q.iter().clone());

while iteration.changed() {

    // Index maintenance
    q_m.from_map(&q, |&(m, n, o)| (m, (n, o)));
    p_bar.from_map(&p, |&((_foo, _bar), _)| (y, x));
    q_no.from_map(&q, |&(m, n, o)| ((n, o), m));
    p_foo.from_map(&p, |&((_foo, _bar), _)| (y, z));
    q_mn.from_map(&q, |&(m, n, o)| ((m, n), o));
    q_n.from_map(&q, |&(m, n, o)| (n, (m, o)));

    // Rules

    // R01: p(x, z) :- p(x, y), p(y, z).
    p.from_join(&p_bar, &p_foo, |&_y, &x, &z| ((x, z), ()));

    // R02: p(x, z) :- p(y, w), u(w, r, z), q(x, r, y).
    p_step_2_1.from_join(&p_bar, &u_s, |&_w, &y, &(r, z)| ((r, y), z));
    p.from_join(&p_step_2_1, &q_no, |&(_r, _y), &z, &x| ((x, z), ()));

    // R03: p(x, z) :- c(y, w, z), p(x, w), p(x, y).
    p_step_3_1.from_join(&c_b, &p_bar, |&_w, &(y, z), &x| ((x, y), z));
    p.from_join(&p_step_3_1, &p, |&(x, _y), &z, _| ((x, z), ()));

    // R04: q(x, r, z) :- p(x, y), q(y, r, z).
    q.from_join(&p_bar, &q_m, |&_y, &x, &(r, z)| (x, r, z));

    // R05: q(x, q, z) :- q(x, r, z), s(r, q).
    q.from_join(&q_n, &s_x, |&_r, &(x, z), &q| (x, q, z));

    // R06: q(x, e, o) :- q(x, y, z), r(y, u, e), q(z, u, o).
    q_step_6_1.from_join(&q_n, &r_d, |&_y, &(x, z), &(u, e)| ((z, u), (x, e)));
    q.from_join(&q_step_6_1, &q_mn, |&(_z, _u), &(x, e), &o| (x, e, o));

    // R07: p(x, z) :- input_p(x, z).
    // `input_p` is a static input, already loaded into `p`.

    // R08: q(m, n, o) :- input_q(m, n, o).
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

    // Note: `r_d` is an indexed version of the input facts `r`
    let r_d: datafrog::Relation<(u32, (u32, u32))> = Vec::new().into();

    // Note: `s_x` is an indexed version of the input facts `s`
    let s_x: datafrog::Relation<(u32, u32)> = Vec::new().into();

    // Note: `u_s` is an indexed version of the input facts `u`
    let u_s: datafrog::Relation<(u32, (u32, u32))> = Vec::new().into();

    // Note: `c_b` is an indexed version of the input facts `c`
    let c_b: datafrog::Relation<(u32, (u32, u32))> = Vec::new().into();

    let (p, q) = {
        let mut iteration = datafrog::Iteration::new();

        // Intensional predicates, and their indices

        let p = iteration.variable::<((u32, u32), ())>("p");

        // Note: `p_bar` is an indexed version of the `p` relation
        let p_bar = iteration.variable::<(u32, u32)>("p_bar");

        // Note: `p_foo` is an indexed version of the `p` relation
        let p_foo = iteration.variable::<(u32, u32)>("p_foo");
        let p_step_2_1 = iteration.variable("p_step_2_1");
        let p_step_3_1 = iteration.variable("p_step_3_1");
        let q = iteration.variable::<(u32, u32, u32)>("q");

        // Note: `q_m` is an indexed version of the `q` relation
        let q_m = iteration.variable::<(u32, (u32, u32))>("q_m");

        // Note: `q_mn` is an indexed version of the `q` relation
        let q_mn = iteration.variable::<((u32, u32), u32)>("q_mn");

        // Note: `q_n` is an indexed version of the `q` relation
        let q_n = iteration.variable::<(u32, (u32, u32))>("q_n");

        // Note: `q_no` is an indexed version of the `q` relation
        let q_no = iteration.variable::<((u32, u32), u32)>("q_no");
        let q_step_6_1 = iteration.variable("q_step_6_1");

        // Duplicate extensional predicates used as `Variable`s

        let c_b = {
            let var = iteration.variable("c_b");
            var.extend(c_b.iter().clone());
            var
        };

        // R07: p(x, z) :- input_p(x, z).
        p.extend(input_p.iter().map(|&tuple| (tuple, ())));

        // R08: q(m, n, o) :- input_q(m, n, o).
        q.extend(input_q.iter().clone());

        while iteration.changed() {
            // Index maintenance
            q_m.from_map(&q, |&(m, n, o)| (m, (n, o)));
            p_bar.from_map(&p, |&((_foo, _bar), _)| (0, 0)); // manual fix
            q_no.from_map(&q, |&(m, n, o)| ((n, o), m));
            p_foo.from_map(&p, |&((_foo, _bar), _)| (0, 0)); // manual fix
            q_mn.from_map(&q, |&(m, n, o)| ((m, n), o));
            q_n.from_map(&q, |&(m, n, o)| (n, (m, o)));

            // Rules

            // R01: p(x, z) :- p(x, y), p(y, z).
            p.from_join(&p_bar, &p_foo, |&_y, &x, &z| ((x, z), ()));

            // R02: p(x, z) :- p(y, w), u(w, r, z), q(x, r, y).
            p_step_2_1.from_join(&p_bar, &u_s, |&_w, &y, &(r, z)| ((r, y), z));
            p.from_join(&p_step_2_1, &q_no, |&(_r, _y), &z, &x| ((x, z), ()));

            // R03: p(x, z) :- c(y, w, z), p(x, w), p(x, y).
            p_step_3_1.from_join(&c_b, &p_bar, |&_w, &(y, z), &x| ((x, y), z));
            p.from_join(&p_step_3_1, &p, |&(x, _y), &z, _| ((x, z), ()));

            // R04: q(x, r, z) :- p(x, y), q(y, r, z).
            q.from_join(&p_bar, &q_m, |&_y, &x, &(r, z)| (x, r, z));

            // R05: q(x, q, z) :- q(x, r, z), s(r, q).
            q.from_join(&q_n, &s_x, |&_r, &(x, z), &q| (x, q, z));

            // R06: q(x, e, o) :- q(x, y, z), r(y, u, e), q(z, u, o).
            q_step_6_1.from_join(&q_n, &r_d, |&_y, &(x, z), &(u, e)| ((z, u), (x, e)));
            q.from_join(&q_step_6_1, &q_mn, |&(_z, _u), &(x, e), &o| (x, e, o));

            // R07: p(x, z) :- input_p(x, z).
            // `input_p` is a static input, already loaded into `p`.

            // R08: q(m, n, o) :- input_q(m, n, o).
            // `input_q` is a static input, already loaded into `q`.
        }

        // The computation results: the completed output relations from the program
        (p.complete(), q.complete())
    };
}
