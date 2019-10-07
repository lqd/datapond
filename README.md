### Datapond

A set of tools for working with [datafrog](https://github.com/rust-lang/datafrog) (until they are ready to be upstreamed there).

All these are made to be used in the limited context of [Polonius](https://github.com/rust-lang/polonius): not in general for all datalog computations. Not to mention, datafrog itself has its peculiarities, and API requirements.

Currently:

1) a datalog-to-datafrog prototyping generator

---

### Datalog-to-datafrog prototyping generator

For _prototyping_ purposes, this will help generate a skeleton of a datafrog computation, from a set of Soufflé-like declarations, and _valid_ datalog rules (there are basically no error checking yet).

The generated skeleton should (hopefully) build as-is, but won't know where to get the data required by the relations. So it can help bootstrap using datafrog, and maybe get 80-90% of the way there.

In the Polonius context, this was used to bootstrap the `Naive` rules variant (the simpler variant) successfully just by filling the relations with data.

Not supported:
- purely extensional joins in rules (datafrog is mostly made for intensional predicate computations), maybe later but we're not using those right now.
- datafrog's API requires intensional predicates to be first in join steps, there should be a warning/error for datalog rules not following this pattern. The skeleton generator will generate a "correct join" (with respect to key and values tuples) but it will not compile. The 2 predicates can easily be swapped if that happens.
- populating extensional indices (unlike intensional indices)
- some kinds of self joins
- leapjoins: those are mostly optimisations over regular joins. Since the purpose of this generator is to easily try different rules and transformations, they can be added later (or manually after generating the skeleton)
