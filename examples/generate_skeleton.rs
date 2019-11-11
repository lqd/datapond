use datapond;

fn main() {
    let text = r#"
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

        // R1
        subset(O1, O2, P) :- outlives(O1, O2, P).

        // R2
        subset(O1, O3, P) :-
          subset(O1, O2, P),
          outlives(O2, O3, P).

        // R3: this is the transitive relation
        equals(O1, O2, P) :-
          subset(O1, O2, P),
          subset(O2, O1, P).

        // R4
        equals(O1, O2, Q) :-
          equals(O1, O2, P),
          cfg_edge(P, Q).

        // R5
        requires(O2, L, P) :-
          requires(O1, L, P),
          equals(O1, O2, P).

        // R6
        requires(O, L, P) :- borrow_region(O, L, P).

        // R7
        requires(O2, L, P) :-
          requires(O1, L, P),
          subset(O1, O2, P).

        // R8
        requires(O, L, Q) :-
          requires(O, L, P),
          !killed(L, P),
          cfg_edge(P, Q),
          region_live_at(O, Q).

        // R9
        borrow_live_at(L, P) :-
          requires(O, L, P),
          region_live_at(O, P).

        // R10
        errors(L, P) :-
          borrow_live_at(L, P),
          invalidates(L, P).
    "#;

    let mut output = String::new();
    datapond::generate_skeleton_datafrog(text, &mut output);
    println!("{}", output);
}
