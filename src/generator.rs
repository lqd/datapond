use crate::data_structures::OrderedMap;
use crate::{ast, parser, typechecker};
use quote::ToTokens;
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt::{self, Write};

type HashMap<K, V> = OrderedMap<K, V>;

/// The representation of what a datalog rule does in datafrog terms
enum Operation {
    StaticMap(),
    DynamicMap(MapStep),
    Join(Vec<JoinStep>),
}

impl ast::Arg {
    fn ident(&self) -> &syn::Ident {
        match self {
            ast::Arg::Ident(ident) => ident,
            _ => unimplemented!("Generator does not support wildcards yet"),
        }
    }
    fn to_string(&self) -> String {
        self.ident().to_string()
    }
}

/// The representation of a join, with the data required to serialize it as Rust code
#[derive(Debug)]
struct JoinStep {
    src_a: String,
    src_b: String,

    is_antijoin: bool,

    key: Vec<String>,
    args: Vec<String>,

    remaining_args_a: Vec<String>,
    remaining_args_b: Vec<String>,

    dest_predicate: String,
    dest_key: Vec<String>,
    dest_args: Vec<String>,
}

#[derive(Debug)]
struct MapStep {
    src_args: String,
    dest_args: String,
}

pub fn generate_skeleton_datafrog(text: &str) -> String {
    let mut output = String::new();
    generate_skeleton_into(text, &mut output);

    // tidy up: filter multiple empty lines in a row
    let filtered = output.replace("\n\n\n", "\n\n");
    filtered
}

fn generate_skeleton_into(text: &str, output: &mut String) {
    // Step 0: parse everything.
    let program = parser::parse(text);
    let program = match typechecker::typecheck(program) {
        Ok(program) => program,
        Err(err) => panic!("Error: {:?} (at {:?})", err, err.span.start()),
    };
    let decls = program.decls;
    let rules = program.rules;

    // Step 1: analyze rules to separate extensional and intensional predicates.
    // These will end up being emitted as datafrog `Relation`s and `Variable`s, respectively.
    let mut intensional = FxHashSet::default();
    let mut extensional = FxHashSet::default();
    for decl in decls.values() {
        if decl.kind != ast::PredicateKind::Input {
            intensional.insert(decl.name.to_string());
        } else {
            extensional.insert(decl.name.to_string());
        }
    }

    // Step 2: visit rules and emit a datafrog "query plan".

    // Actually used predicates and indices
    let mut extensional_inputs = FxHashSet::default();
    let mut intensional_inputs = FxHashSet::default();

    let mut extensional_indices = FxHashMap::default();
    let mut intensional_indices = FxHashMap::default();

    // All relations used as keys need to be encoded as `((K, V), ())` tuples,
    // as the joins are done on the keys.
    let mut predicates_consumed_as_keys = FxHashSet::default();

    // The skeleton code:
    // - the inital data loading, before the loop, to fill the `Variable`s with
    //   data from the `Relation`s they join with in the rules.
    // - the dynamic computation data: the loop itself, executing the joins
    let mut generated_code_static_input: Vec<String> = Vec::new();
    let mut generated_code_dynamic_computation: Vec<String> = Vec::new();

    let mut operations = Vec::new();

    // Generate an `Operation` per rule, describing what the rule does, and
    // the data required to serialize it as rust code later. This is done in 2 steps
    // because we need to know which predicates are used as complete keys _before_
    // serializing them to code: the tuple produced by each rule would be different
    // depending on the join key of later rules.
    for (rule_idx, rule) in rules.iter().enumerate() {
        let body: Vec<_> = rule.body.iter().collect();

        let operation = match body.len() {
            0 => unreachable!(),

            1 => {
                // This a `map` operation.

                // Record used inputs to filter code generation later
                if !intensional.contains(&body[0].predicate.to_string()) {
                    extensional_inputs.insert(body[0].predicate.to_string());
                }

                let operation = {
                    // If this is mapping over an extensional predicate, we can emit
                    // this outside of the datalog computation loop, since the input is static.
                    if extensional.contains(&body[0].predicate.to_string()) {
                        Operation::StaticMap()
                    } else {
                        // otherwise, it's a map during computation

                        let args_a: Vec<_> = rule.head.args.clone();
                        let args_b: Vec<_> = body[0].args.clone();

                        let src_args = args_b
                            .iter()
                            .map(|arg: &ast::Arg| {
                                let arg = arg.ident();
                                if args_a.contains(arg) {
                                    arg.to_string().to_lowercase()
                                } else {
                                    format!("_{}", arg.to_string().to_lowercase())
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ");

                        let mut dest_args = args_a
                            .iter()
                            .map(|arg| {
                                if args_a.contains(arg) {
                                    arg.to_string().to_lowercase()
                                } else {
                                    format!("_{}", arg.to_string().to_lowercase())
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ");

                        if args_a.len() == 1 {
                            dest_args = format!("{}, ()", dest_args);
                        }

                        let operation = MapStep {
                            src_args,
                            dest_args,
                        };
                        Operation::DynamicMap(operation)
                    }
                };
                operation
            }

            _ => {
                // This is a `join` operation

                // TODO: check if there is only one intensional predicate and the rest are extensional
                // so that we can output a leapjoin instead of a regular join

                let mut steps: Vec<JoinStep> = Vec::new();

                for (literal_idx, literal) in body.iter().enumerate().skip(1) {
                    // We're joining 2 literals, but a step at a time (1 literal at a time),
                    // using the previous join output with the current step's literal.
                    // So the first `step_idx` of the join will start at `literal_idx` 1,
                    // joining `body[0]` and `body[1]`.
                    let step_idx = literal_idx - 1;

                    let is_first_step = step_idx == 0;
                    let is_last_step = literal_idx == body.len() - 1;

                    // TODO: datafrog has requirements that the joined Variable is the first
                    // argument, so if this is the first (or only) step of a join, the second literal
                    // should be a Variable (or emit an error), and swap the order here, or maybe also
                    // emit an error asking to swap in the source directly.

                    // When we're at the first step, there is no previous step result with which
                    // to join. But when we're at `literal_idx` of at least 2, we've joined 2
                    // literals already and continue to join that result, with the current step's literal.
                    let mut previous_step = if is_first_step {
                        None
                    } else {
                        Some(&mut steps[step_idx - 1])
                    };

                    // The destination where we produce our join's tuples can be:
                    // - a temporary relation, when we're at an intermediary of
                    //   the multiple-step join
                    // - the rule's conclusion when we're on the last step
                    let dest_predicate = if is_last_step {
                        rule.head.predicate.to_string()
                    } else {
                        format!(
                            "{}_step_{}_{}",
                            rule.head.predicate,
                            rule_idx + 1,
                            step_idx + 1
                        )
                    };

                    // Record used inputs to filter code generation later
                    intensional.insert(dest_predicate.clone());

                    // The arguments to the source literals can either come from the
                    // first 2 literals in the body (at the firs step of the join),
                    // or from the previous step's result and current step's literal.
                    let args_a: Vec<String> = if let Some(ref mut previous_step) = previous_step {
                        previous_step
                            .key
                            .iter()
                            .chain(previous_step.args.iter())
                            .map(|v| v.to_string())
                            .collect()
                    } else {
                        body[0].args.iter().map(|a| a.to_string()).collect()
                    };

                    let args_b: Vec<_> = literal.args.iter().map(|a| a.to_string()).collect();

                    // The join key is the shared variables between the 2 relations
                    let mut key: Vec<_> = args_b
                        .iter()
                        .map(|v| v.to_string())
                        .filter(|v| args_a.contains(v))
                        .collect();

                    // We now need to know which arguments were not used in the key: they will be the
                    // arguments that the datafrog closure producing the tuples of the join
                    // will _receive_.
                    let is_arg_used_later = |arg: &str, skip| {
                        // if the argument is used in later steps, we need to retain it
                        for literal in body.iter().skip(skip) {
                            if literal.args.iter().any(|a| &a.to_string() == arg) {
                                return true;
                            }
                        }

                        // similarly, if the variable is produced by the join process itself
                        if rule.head.args.iter().any(|a| &a.to_string() == arg) {
                            return true;
                        }

                        // else, the argument is unused, and we can avoid producing it at this join step
                        false
                    };

                    let remaining_args_a: Vec<String> = args_a
                        .iter()
                        .filter(|v| !key.contains(v))
                        .filter(|v| is_arg_used_later(v, step_idx + 1))
                        .map(|v| v.to_string())
                        .collect();
                    let remaining_args_b: Vec<String> = args_b
                        .iter()
                        .filter(|v| !key.contains(v))
                        .filter(|v| is_arg_used_later(v, step_idx + 1))
                        .map(|v| v.to_string())
                        .collect();

                    // This step's arguments, which will be used by the next step when computing
                    // its join key.
                    let mut args = Vec::new();
                    for arg in remaining_args_a.iter().chain(remaining_args_b.iter()) {
                        args.push(arg.clone());
                    }

                    // Compute the source predicates:
                    // - if we're at the first step it'll be the first 2 literals
                    // - if we're at a later step, it'll be the previous step result, and the
                    //  current literal
                    //
                    // In both cases, predicates can be joined via some index, when only some of
                    // the arguments in the key are used. In this case, either source index can be
                    // used instead of the relation with full tuples.
                    //
                    // The "left" relation in the join could come from the previous step,
                    // in that case, there is no specific index to lookup.
                    //
                    let src_a = if let Some(ref mut previous_step) = previous_step {
                        previous_step.dest_predicate.clone()
                    } else {
                        if remaining_args_a.is_empty() {
                            body[0].predicate.to_string()
                        } else {
                            generate_index_relation(
                                &decls,
                                &body[0],
                                &args_a,
                                &key,
                                &remaining_args_a,
                                &mut extensional,
                                &mut extensional_indices,
                                &mut intensional,
                                &mut intensional_inputs,
                                &mut intensional_indices,
                            )
                        }
                    };

                    let mut src_b = if remaining_args_b.is_empty() {
                        literal.predicate.to_string()
                    } else {
                        generate_index_relation(
                            &decls,
                            &literal,
                            &args_b,
                            &key,
                            &remaining_args_b,
                            &mut extensional,
                            &mut extensional_indices,
                            &mut intensional,
                            &mut intensional_inputs,
                            &mut intensional_indices,
                        )
                    };

                    // Self-joins with intensional predicates consumed as keys need to be
                    // special-cased in datafrog: the same arguments are used, but in their own order,
                    // so we need an index with that  order for the tuple-keys. The join key is also unused,
                    // as the whole tuple is the key in its canonical order (I think ?) or the order of the
                    // left element. Those cases are rare, so let's handle it the way I know
                    // works in our Polonius use-cases.
                    if src_a == src_b && remaining_args_a.is_empty() && remaining_args_b.is_empty()
                    {
                        // A self join with the same ordering would be a no-op, so look for the element
                        // which has the canonical ordering

                        let canonical_args = decls[&src_a]
                            .parameters
                            .iter()
                            .map(|decl| decl.name.to_string().to_uppercase());
                        let canonicalized_args_a = args_a.iter().map(|arg| arg.to_string());
                        if canonicalized_args_a.eq(canonical_args) {
                            // The left element has the canonical ordering, the right element needs to be indexed
                            // in a new wrapped-tuple relation

                            let relation_args = &args_b;
                            let value_args = Vec::new();

                            let index_relation = generate_index_relation_name(
                                &decls,
                                &literal.predicate,
                                &args_a,
                                relation_args,
                            );

                            // Index maintenance
                            if extensional.contains(&literal.predicate.to_string()) {
                                record_extensional_index_use(
                                    &decls,
                                    &literal.predicate,
                                    &index_relation,
                                    relation_args,
                                    &args_a,
                                    &value_args,
                                    &mut extensional,
                                    &mut extensional_indices,
                                );
                            } else {
                                record_intensional_index_use(
                                    &literal,
                                    &args_b,
                                    &value_args,
                                    &index_relation,
                                    &mut intensional,
                                    &mut intensional_inputs,
                                    &mut intensional_indices,
                                );
                            }

                            src_b = index_relation;
                            key = args_a;
                        } else {
                            // The right element has the canonical ordering, the left element needs to be indexed
                            // in a new wrapped-tuple relation
                            unimplemented!("no case currently hits this ?");
                        }
                    }

                    // The arguments that the datafrog closure will need to _produce_.
                    // Since these are only known in the next step, the next loop iteration
                    // will fill them. When we're at the last step, we produce what the rule
                    // asked us to produce in the first place.
                    let dest_args = if is_last_step {
                        rule.head.args.iter().map(|a| a.to_string()).collect()
                    } else {
                        Vec::new()
                    };

                    // Now that we have computed what this join step requires from the previous step,
                    // we can back patch the previous one, to tell it what key-value tuples to produce.
                    if let Some(ref mut previous_step) = previous_step {
                        previous_step.dest_key = key.clone();
                        previous_step.dest_args = remaining_args_a.clone();
                    }

                    let is_antijoin = literal.is_negated;

                    if !is_antijoin {
                        if remaining_args_a.is_empty() {
                            predicates_consumed_as_keys.insert(src_a.clone());
                        }

                        if remaining_args_b.is_empty() {
                            predicates_consumed_as_keys.insert(src_b.clone());
                        }
                    }

                    let step = JoinStep {
                        src_a,
                        src_b,
                        is_antijoin,
                        key,
                        args,
                        remaining_args_a,
                        remaining_args_b,
                        dest_predicate,
                        dest_key: Vec::new(),
                        dest_args,
                    };

                    steps.push(step);
                }

                Operation::Join(steps)
            }
        };

        operations.push(operation);
    }

    // Serialize rule operations as string to generate the skeleton code
    for (rule_idx, (rule, operation)) in rules.iter().zip(operations.into_iter()).enumerate() {
        let rule_id = format!("R{:02}", rule_idx + 1);
        let rule_comment = format!("// {}: {}", rule_id, rule);

        generated_code_dynamic_computation.push(rule_comment.clone());

        match operation {
            Operation::StaticMap() => {
                // A `map` operation depends on:
                // - whether the predicate was consumed as a key, where we can simply
                //   wrap the tuple
                // - whether body already projects to what the rule expects, where we can simply
                //   clone the input
                // - all the other cases, where we'll do the projection of the input relation

                // static `map` operations are composed of a projection of a single relation
                assert!(rule.body.len() == 1);

                let projection_matches_head = rule
                    .head
                    .args
                    .iter()
                    .eq(rule.body[0].args.iter().map(|x| x.ident()));

                // The encoding of predicates consumed as keys requires to
                // wrap the key-value tuple as a key in another tuple, and a unit value.
                let produced_tuple = if predicates_consumed_as_keys
                    .contains(&rule.head.predicate.to_string())
                {
                    "map(|&tuple| (tuple, ()))".to_string()
                } else if projection_matches_head {
                    // If the projection matches the head of the rule, we can simply clone the input
                    // (as the arguments are the same).
                    "clone()".to_string()
                } else {
                    // If the body arguments do not match the head of the rule, we need to `map` over
                    // the input to project what the rule expects to output.
                    let source_args: Vec<_> =
                        rule.body[0].args.iter().map(|a| a.to_string()).collect();
                    let target_args: Vec<_> =
                        rule.head.args.iter().map(|a| a.to_string()).collect();

                    let tupled_src = join_args_as_tuple(&source_args, &target_args, &target_args);
                    let tupled_target =
                        join_args_as_tuple(&target_args, &target_args, &target_args);
                    format!("map(|&{}| {})", tupled_src, tupled_target)
                };

                let operation = format!(
                    "{}.extend({}.iter().{});",
                    rule.head.predicate, rule.body[0].predicate, produced_tuple
                );

                generated_code_static_input.push(rule_comment);
                generated_code_static_input.push(operation);

                generated_code_dynamic_computation.push(format!(
                    "// `{}` is a static input, already loaded into `{}`.",
                    rule.body[0].predicate, rule.head.predicate,
                ));
            }
            Operation::DynamicMap(step) => {
                warn!(
                    "warning: untested code generation! dynamic map step: {:?}",
                    step
                );

                // The encoding of these predicates consumed as keys requires to
                // wrap the key-value tuple as a key in another tuple, and a unit value.
                let src_args =
                    if predicates_consumed_as_keys.contains(&rule.head.predicate.to_string()) {
                        format!("({}), _", step.src_args)
                    } else {
                        step.src_args
                    };

                let operation = format!(
                    "{dest}.from_map(&{src}, |&({src_args})| ({dest_args}));",
                    dest = rule.head.predicate,
                    src = rule.body[0].predicate,
                    src_args = src_args,
                    dest_args = step.dest_args,
                );
                generated_code_dynamic_computation.push(operation);

                unimplemented!("no case currently hits this ?");
            }
            Operation::Join(steps) => {
                for (step_idx, step) in steps.iter().enumerate() {
                    let is_last_step = step_idx == steps.len() - 1;

                    // Stringify the datafrog join closure arguments:
                    // - the key
                    // - the unused arguments from the first relation
                    // - the unused arguments from the second relation
                    let tupled_src_key =
                        join_args_as_tuple(&step.key, &step.dest_key, &step.dest_args);

                    let tupled_args_a = match step.remaining_args_a.len() {
                        0 => "_".to_string(),
                        _ => format!(
                            "&{}",
                            join_args_as_tuple(
                                &step.remaining_args_a,
                                &step.dest_key,
                                &step.dest_args
                            )
                        ),
                    };

                    let tupled_args_b = match step.remaining_args_b.len() {
                        0 => "_".to_string(),
                        _ => format!(
                            "&{}",
                            join_args_as_tuple(
                                &step.remaining_args_b,
                                &step.dest_key,
                                &step.dest_args
                            )
                        ),
                    };

                    // Stringify the datafrog closure body: the value it will produce, and which can be
                    // a simple value, or a key-value tuple, depending on the join step, and the destination
                    // relation layout.
                    let mut produced_tuple = {
                        if is_last_step {
                            // we're on the last step, so we must produce what the rule's conclusion expects
                            step.dest_args.join(", ").to_lowercase()
                        } else {
                            // we're at an intermediary step of the multi-step join, so we must produce
                            // what the next step expects
                            let tupled_dest_key =
                                join_args_as_tuple(&step.dest_key, &step.dest_key, &step.dest_args);
                            let tupled_dest_args = join_args_as_tuple(
                                &step.dest_args,
                                &step.dest_key,
                                &step.dest_args,
                            );
                            format!("{}, {}", tupled_dest_key, tupled_dest_args)
                        }
                    };

                    // The encoding of these predicates consumed as keys requires to
                    // wrap the key-value tuple as a key in another tuple, and a unit value.
                    if predicates_consumed_as_keys.contains(&step.dest_predicate) {
                        produced_tuple = format!("({}), ()", produced_tuple);
                    }

                    let operation = if step.is_antijoin { "antijoin" } else { "join" };

                    // Adapt the closure signature to the specific join, we're doing. Antijoins
                    // consume all arguments, there will be no unused arguments for the join closure
                    // to receive.
                    let args = if step.is_antijoin {
                        tupled_args_a
                    } else {
                        format!(
                            "{args_a}, {args_b}",
                            args_a = tupled_args_a,
                            args_b = tupled_args_b,
                        )
                    };

                    // If either predicates is not intensional: it's either a declared extensional
                    // predicate, or one which was generated as an index of a declared relation,
                    // we'll record its use to only emit actually used `Relation`s.
                    // Technically, extensional predicates can only appear in the right element
                    // of a regular join; but we can reorder, and have to handle leapjoins, so let's
                    // check both right and left elements.
                    record_predicate_use(
                        &step.src_a,
                        &intensional,
                        &mut extensional_inputs,
                        &mut intensional_inputs,
                    );
                    record_predicate_use(
                        &step.src_b,
                        &intensional,
                        &mut extensional_inputs,
                        &mut intensional_inputs,
                    );

                    let operation = format!(
                        "{dest}.from_{operation}(&{src_a}, &{src_b}, |&{key}, {args}| ({tuple}));",
                        dest = step.dest_predicate,
                        operation = operation,
                        src_a = step.src_a,
                        src_b = step.src_b,
                        key = tupled_src_key,
                        args = args,
                        tuple = produced_tuple,
                    );
                    generated_code_dynamic_computation.push(operation);
                }
            }
        }

        // Add an empty line after every datalog rule conversion
        if rule_idx < rules.len() - 1 {
            generated_code_dynamic_computation.push("".to_string());
        }
    }

    // Infer the output of the computation: the difference between all the intensional
    // predicates and the ones used as inputs.
    let main_relation_candidates: Vec<_> = intensional
        .difference(&intensional_inputs)
        .cloned()
        .collect();

    println!(
        "{} extensional predicates/indices used (out of {}) and which can be a datafrog `Relation`:",
        extensional_inputs.len(),
        extensional.len(),
    );
    let mut extensional: Vec<_> = extensional_inputs.into_iter().collect();
    extensional.sort();
    for (idx, relation) in extensional.iter().enumerate() {
        let is_index = match extensional_indices.get(relation) {
            Some((original_predicate, ..)) => format!(" (index on `{}`)", original_predicate),
            None => "".to_string(),
        };

        println!("{:02}: `{}`{}", idx + 1, relation, is_index);
    }

    println!(
        "\n{} intensional predicates (including {} indices) requiring a datafrog `Variable`:",
        intensional.len(),
        intensional_indices.len(),
    );

    let mut intensional: Vec<_> = intensional.into_iter().collect();
    intensional.sort();
    for (idx, variable) in intensional.iter().enumerate() {
        let is_index = match intensional_indices.get(variable) {
            Some((original_literal, ..)) => format!(" (index on `{}`)", original_literal.predicate),
            None => "".to_string(),
        };

        println!("{:02}: `{}`{}", idx + 1, variable, is_index);
    }

    generate_skeleton_code(
        output,
        decls,
        extensional,
        extensional_indices,
        intensional,
        intensional_indices,
        predicates_consumed_as_keys,
        main_relation_candidates,
        generated_code_static_input,
        generated_code_dynamic_computation,
    )
    .expect("Skeleton code generation failed");
}

fn generate_skeleton_code(
    output: &mut String,
    decls: HashMap<String, ast::PredicateDecl>,
    extensional_predicates: Vec<String>,
    extensional_indices: FxHashMap<String, (&syn::Ident, String)>,
    intensional_predicates: Vec<String>,
    intensional_indices: FxHashMap<String, (&ast::Literal, Vec<String>, Vec<String>)>,
    predicates_consumed_as_keys: FxHashSet<String>,
    main_relation_candidates: Vec<String>,
    generated_code_static_input: Vec<String>,
    generated_code_dynamic_computation: Vec<String>,
) -> fmt::Result {
    writeln!(output, "")?;
    writeln!(output, "// Extensional predicates, and their indices")?;
    writeln!(output, "")?;

    for relation in extensional_predicates.iter() {
        if let Some(arg_decls) = decls.get(relation) {
            // This is one the initial extensional predicates
            let arg_types: Vec<_> = arg_decls
                .parameters
                .iter()
                .map(|decl| decl.typ_as_string())
                .collect();

            let arg_types = if predicates_consumed_as_keys.contains(relation) {
                format!("({}), ()", arg_types.join(", "))
            } else {
                arg_types.join(", ")
            };

            writeln!(
                output,
                "let {relation}: Relation<({arg_types})> = Vec::new().into();",
                relation = relation,
                arg_types = arg_types,
            )?;
        } else {
            // This is an index over an extensional predicate
            let (original_predicate, arg_types) = &extensional_indices[relation];

            let arg_types = if predicates_consumed_as_keys.contains(relation) {
                format!("({}), ()", arg_types)
            } else {
                arg_types.clone()
            };

            writeln!(output, "")?;
            writeln!(
                output,
                "// Note: `{relation}` is an indexed version of the input facts `{original_predicate}`",
                relation = relation,
                original_predicate = original_predicate,
            )?;
            writeln!(
                output,
                "let {relation}: Relation<({arg_types})> = Vec::new().into();",
                relation = relation,
                arg_types = arg_types,
            )?;
            writeln!(output, "")?;
        }
    }

    writeln!(output, "")?;

    // There can be only one 'main' intensional predicate
    if main_relation_candidates.len() == 1 {
        let main = &main_relation_candidates[0];
        writeln!(output, "// `{}` inferred as the output relation", main)?;
        writeln!(output, "let {} = {{", main)?;
    } else {
        writeln!(
            output,
            "// Note: couldn't infer output relation automatically"
        )?;
    }

    writeln!(output, "")?;
    writeln!(output, "let mut iteration = Iteration::new();")?;

    writeln!(output, "")?;
    writeln!(output, "// Intensional predicates, and their indices")?;
    writeln!(output, "")?;

    for variable in intensional_predicates.iter() {
        if let Some(arg_decls) = decls.get(variable) {
            // This is one of the initial intensional predicates
            let arg_types: Vec<_> = arg_decls
                .parameters
                .iter()
                .map(|decl| decl.typ_as_string())
                .collect();

            // The encoding of these predicates consumed as keys requires to
            // wrap the key-value tuple as a key in another tuple, and a unit value.
            let arg_types = if predicates_consumed_as_keys.contains(variable) {
                format!("({}), ()", arg_types.join(", "))
            } else {
                arg_types.join(", ")
            };

            writeln!(
                output,
                "let {variable} = iteration.variable::<({arg_types})>({variable:?});",
                variable = variable,
                arg_types = arg_types,
            )?;
        } else if let Some((original_literal, key, args)) = intensional_indices.get(variable) {
            // This is an index over an intensional predicate
            let original_predicate = &original_literal.predicate;

            writeln!(output, "")?;
            writeln!(output,
                "// Note: `{variable}` is an indexed version of the `{original_predicate}` relation",
                variable = variable,
                original_predicate = original_predicate,
            )?;

            let arg_names = original_literal
                .args
                .iter()
                .map(|a| a.to_string())
                .collect();
            let key_types: Vec<_> = key
                .iter()
                .map(|v| {
                    canonicalize_arg_type(&decls, original_predicate, &arg_names, v).to_string()
                })
                .collect();
            let args_types: Vec<_> = args
                .iter()
                .map(|v| {
                    canonicalize_arg_type(&decls, original_predicate, &arg_names, v).to_string()
                })
                .collect();

            // The encoding of these predicates consumed as keys requires to
            // wrap the key-value tuple as a key in another tuple, and a unit value.
            let variable_type = join_types_as_tuple(key_types, args_types);
            let variable_type = if predicates_consumed_as_keys.contains(variable) {
                format!("{}, ()", variable_type)
            } else {
                variable_type
            };

            writeln!(
                output,
                "let {variable} = iteration.variable::<({variable_type})>({variable:?});",
                variable = variable,
                variable_type = variable_type,
            )?;
        } else {
            // This is an intermediary step variable used in joins
            writeln!(
                output,
                "let {variable} = iteration.variable({variable:?});",
                variable = variable
            )?;
        }
    }

    // Initial data loading
    writeln!(output, "")?;
    for data_loading_operation in generated_code_static_input.chunks(2) {
        // Static data-loading generates pairs of lines:
        // - a comment describing the rule the data-loading operation is for
        // - the data-loading operation itself: the code creating a datafrog `Relation`
        let rule_comment = &data_loading_operation[0];
        writeln!(output, "{}", rule_comment)?;

        let code = &data_loading_operation[1];
        writeln!(output, "{}\n", code)?;
    }

    writeln!(output, "while iteration.changed() {{")?;

    // Intensional indices maintenance
    writeln!(output, "")?;
    writeln!(output, "    // Index maintenance")?;
    for (index_relation, (indexed_literal, key, args)) in intensional_indices.iter() {
        let original_relation = &indexed_literal.predicate;

        let mut produced_key = join_args_as_tuple(&key, &key, &args);
        let mut produced_args = join_args_as_tuple(&args, &key, &args);

        let arg_decls = &decls[&original_relation.to_string()];
        let declared_args: Vec<_> = arg_decls
            .parameters
            .iter()
            .map(|decl| decl.name.to_string().to_lowercase())
            .collect();

        // The encoding of these predicates consumed as keys is a tuple
        // wrapping the key-value tuple as a key in another tuple, and a unit value, so we need to
        // take care of the unit value to correctly read the source tuples.
        let relation_args = if predicates_consumed_as_keys.contains(&original_relation.to_string())
        {
            let relation_args = join_args_as_tuple(&declared_args, &key, &args);
            format!("({}, _)", relation_args)
        } else {
            let arg_names: Vec<_> = indexed_literal.args.iter().map(|v| v.to_string()).collect();

            let canonicalized_key: Vec<_> = key
                .iter()
                .map(|v| canonicalize_arg_name(&decls, &indexed_literal.predicate, &arg_names, v))
                .collect();

            let canonicalized_args: Vec<_> = args
                .iter()
                .map(|v| canonicalize_arg_name(&decls, &indexed_literal.predicate, &arg_names, v))
                .collect();

            produced_key =
                join_args_as_tuple(&canonicalized_key, &canonicalized_key, &canonicalized_args);
            produced_args =
                join_args_as_tuple(&canonicalized_args, &canonicalized_key, &canonicalized_args);

            let relation_args =
                join_args_as_tuple(&declared_args, &canonicalized_key, &canonicalized_args);

            relation_args
        };

        writeln!(output,
            "    {index_relation}.from_map(&{original_relation}, |&{relation_args}| ({produced_key}, {produced_args}));",
            index_relation = index_relation,
            original_relation = original_relation,
            relation_args = relation_args,
            produced_key = produced_key,
            produced_args = produced_args,
        )?;
    }

    // Finally, output the computation rules
    writeln!(output, "")?;
    writeln!(output, "    // Rules")?;
    writeln!(output, "")?;
    for line in generated_code_dynamic_computation {
        if line.is_empty() {
            writeln!(output, "")?;
        } else {
            writeln!(output, "    {}", line)?;
        }
    }

    writeln!(output, "}}")?;

    if main_relation_candidates.len() == 1 {
        writeln!(output, "")?;
        writeln!(output, "{}.complete()", main_relation_candidates[0])?;
        writeln!(output, "}};")?;
    }

    Ok(())
}

fn generate_index_relation<'a>(
    decls: &HashMap<String, ast::PredicateDecl>,
    literal: &'a ast::Literal,   // the literal being indexed
    relation_args: &Vec<String>, // the order and names of the arguments of the index
    key_args: &Vec<String>,      // the arguments used in the index key
    value_args: &Vec<String>,    // the arguments used in the index value
    extensional_predicates: &mut FxHashSet<String>,
    extensional_indices: &mut FxHashMap<String, (&'a syn::Ident, String)>,
    intensional_predicates: &mut FxHashSet<String>,
    intensional_inputs: &mut FxHashSet<String>,
    intensional_indices: &mut FxHashMap<String, (&'a ast::Literal, Vec<String>, Vec<String>)>,
) -> String {
    let index_relation =
        generate_index_relation_name(&decls, &literal.predicate, &key_args, &relation_args);

    // Index maintenance
    if extensional_predicates.contains(&literal.predicate.to_string()) {
        record_extensional_index_use(
            decls,
            &literal.predicate,
            &index_relation,
            &relation_args,
            &key_args,
            &value_args,
            extensional_predicates,
            extensional_indices,
        );
    } else {
        record_intensional_index_use(
            &literal,
            &key_args,
            &value_args,
            &index_relation,
            intensional_predicates,
            intensional_inputs,
            intensional_indices,
        );
    }

    index_relation
}

fn record_predicate_use(
    predicate: &str,
    intensional_predicates: &FxHashSet<String>,
    extensional_inputs: &mut FxHashSet<String>,
    intensional_inputs: &mut FxHashSet<String>,
) {
    if !intensional_predicates.contains(predicate) {
        extensional_inputs.insert(predicate.to_string());
    } else {
        intensional_inputs.insert(predicate.to_string());
    }
}

fn record_extensional_index_use<'a>(
    decls: &HashMap<String, ast::PredicateDecl>,
    origin_predicate: &'a syn::Ident, // the relation over which the index relation maps
    index_predicate: &str,            // the index relation
    index_args: &Vec<String>,         // the index arguments name and order
    key_args: &Vec<String>,           // the indexed arguments used in the "key"
    value_args: &Vec<String>,         // the indexed arguments used in the "value"
    extensional_predicates: &mut FxHashSet<String>,
    extensional_indices: &mut FxHashMap<String, (&'a syn::Ident, String)>,
) {
    // Canonicalize the types of the keys and value arguments
    let key_types: Vec<_> = key_args
        .iter()
        .map(|v| canonicalize_arg_type(&decls, origin_predicate, index_args, v).to_string())
        .collect();
    let arg_types: Vec<_> = value_args
        .iter()
        .map(|v| canonicalize_arg_type(&decls, origin_predicate, index_args, v).to_string())
        .collect();

    extensional_predicates.insert(index_predicate.to_string());
    extensional_indices.insert(
        index_predicate.to_string(),
        (origin_predicate, join_types_as_tuple(key_types, arg_types)),
    );
}

fn record_intensional_index_use<'a>(
    literal: &'a ast::Literal,
    key_args: &Vec<String>,
    value_args: &Vec<String>,
    index_relation: &str,
    intensional_predicates: &mut FxHashSet<String>,
    intensional_inputs: &mut FxHashSet<String>,
    intensional_indices: &mut FxHashMap<String, (&'a ast::Literal, Vec<String>, Vec<String>)>,
) {
    // When using an index, we're effectively using both `Variables`
    intensional_predicates.insert(index_relation.to_string());
    intensional_inputs.insert(literal.predicate.to_string());

    intensional_indices.insert(
        index_relation.to_string(),
        (literal, key_args.clone(), value_args.clone()),
    );
}

fn find_arg_decl<'a>(
    global_decls: &'a HashMap<String, ast::PredicateDecl>,
    predicate: &syn::Ident,
    args: &Vec<String>,
    variable: &str,
) -> &'a ast::ParamDecl {
    let idx = args.iter().position(|arg| arg == variable).expect(&format!(
        "Couldn't find variable {:?} in the specified args: {:?}",
        variable, args
    ));

    let predicate_arg_decls = &global_decls[&predicate.to_string()];
    let arg_decl = &predicate_arg_decls.parameters[idx];
    arg_decl
}

// Find the canonical names of arguments via their usage in the indexed relation.
// For example, when indexing `outlives(o1, o2, p)` via `outlives(o2, o3, p)` in a join,
// we need to map the local names in the join to their original relation names and order,
// to find that the index's `(o2, o3, p)` is mapping over `(o1, o2, p)`.
fn canonicalize_arg_name(
    global_decls: &HashMap<String, ast::PredicateDecl>,
    predicate: &syn::Ident,
    args: &Vec<String>,
    variable: &str,
) -> String {
    find_arg_decl(global_decls, predicate, args, variable)
        .name
        .to_string()
        .to_lowercase()
}

// Find the canonical types of arguments via their usage in the indexed relation.
// For example, when indexing `outlives(o1, o2, p)` via `outlives(o2, o3, p)` in a join,
// we need to map the local names in the join to their original relation names and order,
// to find that the index's `(o2, o3, p)` is mapping over `(o1, o2, p)`, to finally find
// these canonical arguments' types and generate a `Relation<(Origin, Origin, Point)>`.
fn canonicalize_arg_type<'a>(
    global_decls: &'a HashMap<String, ast::PredicateDecl>,
    predicate: &syn::Ident,
    args: &Vec<String>,
    variable: &str,
) -> String {
    find_arg_decl(global_decls, predicate, args, variable)
        .typ
        .to_token_stream()
        .to_string()
}

fn generate_index_relation_name(
    decls: &HashMap<String, ast::PredicateDecl>,
    predicate: &syn::Ident,
    key: &Vec<String>,
    args: &Vec<String>,
) -> String {
    let mut index_args = String::new();
    for v in key {
        let idx_key = canonicalize_arg_name(&decls, predicate, &args, v);
        index_args.push_str(&idx_key.to_string());
    }

    format!("{}_{}", predicate, index_args)
}

/// Generate tupled rust names for the datalog arguments, potentially prefixed
/// with _ to avoid generating a warning when it's not actually used
/// to produce the tuple, and potentially "untupled" if there's only one.
fn join_args_as_tuple(
    variables: &Vec<String>,
    uses_key: &Vec<String>,
    uses_args: &Vec<String>,
) -> String {
    let name_arg = |arg| {
        if uses_key.contains(arg)
            || uses_key.contains(&arg.to_uppercase())
            || uses_args.contains(arg)
            || uses_args.contains(&arg.to_uppercase())
        {
            arg.to_string().to_lowercase()
        } else {
            format!("_{}", arg.to_string().to_lowercase())
        }
    };

    if variables.len() == 1 {
        name_arg(&variables[0])
    } else {
        format!(
            "({})",
            variables
                .iter()
                .map(name_arg)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn join_types_as_tuple(key_types: Vec<String>, args_types: Vec<String>) -> String {
    let join_as_tuple = |types: Vec<String>| {
        if types.len() == 1 {
            types[0].to_string()
        } else {
            format!("({})", types.into_iter().collect::<Vec<_>>().join(", "))
        }
    };

    let tupled_key_types = join_as_tuple(key_types);

    if args_types.is_empty() {
        format!("{}", tupled_key_types)
    } else {
        let tupled_args_types = join_as_tuple(args_types);
        format!("{}, {}", tupled_key_types, tupled_args_types)
    }
}
