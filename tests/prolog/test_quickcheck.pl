:- use_module('../../quicktest/quickcheck').
:- use_module(library(plunit)).
:- use_module(library(settings), [set_setting/2, setting/2]).
:- use_module(support).
:- use_module(properties).

:- begin_tests(quickcheck, [setup(set_random(seed(1)))]).

run(Property, Args, Result) :-
    run(100, Property, Args, Result).

run(Count, Property, Args, Result) :-
    quickcheck:run_tests(Count, qc_props, Property, Args, Result).

run_quickcheck(Spec, Json) :-
    with_temp_dir(( ignore(quickcheck(Spec)),
                    read_result(Json)
                  )).

test(true_property_gives_ok) :-
    run(reverse_twice, [_:list(integer)], ok).

test(false_property_gives_fail, [true(Example = [X:integer]), true(X >= 100)]) :-
    run(small_integer, [_:integer], fail(Example)).

test(counterexample_is_unified_with_property_types,
     true(Example = [_:integer, _:integer])) :-
    run(less_than, [_:integer, _:integer], fail(Example)).

test(counterexample_falsifies_the_property, [forall(between(1, 5, Seed))]) :-
    set_random(seed(Seed)),
    run(small_integer, [_:integer], fail([X:integer])),
    \+ small_integer(X:integer).

test(counterexample_is_shrunk_in_every_argument,
     true(Example == [0:integer, 0:integer])) :-
    run(less_than, [_:integer, _:integer], fail(Example)).

test(counterexample_list_shrinks_to_one_element,
     true(Example == [[0]:list(integer)])) :-
    run(lossy_reverse_keeps_length, [_:list(integer)], fail(Example)).

test(zero_tests_gives_ok) :-
    run(0, always_fails, [_:integer], ok).

test(exception_in_property_is_not_caught, throws(my_exception)) :-
    run(throws_term, [_:integer], _).

test(error_in_property_is_not_caught,
     throws(error(instantiation_error, _))) :-
    run(throws_error, [_:integer], _).

test(unsupported_type_is_not_ok) :-
    catch(run(unsupported, [_:no_such_type], Result), _, Result = error),
    Result \== ok.

% quickcheck.py compares accepted with the string "true".
test(quickcheck_writes_result_for_true_property) :-
    run_quickcheck(qc_props:reverse_twice/1, Json),
    _{ accepted:"true",
       name:"reverse_twice/1",
       testcount:100,
       counterparams:[],
       counterterm:"null"
     } :< Json.

test(quickcheck_fails_and_writes_counterexample) :-
    run_quickcheck(qc_props:lossy_reverse_keeps_length/1, Json),
    _{ accepted:"false",
       name:"lossy_reverse_keeps_length/1",
       counterparams:[_{type:"list(integer)", value:"[0]"}],
       counterterm:"lossy_reverse_keeps_length([0])"
     } :< Json.

test(quickcheck_counterparams_follow_argument_order) :-
    run_quickcheck(qc_props:less_than/2, Json),
    _{ counterparams:[_{type:"integer", value:"0"}, _{type:"integer", value:"0"}],
       counterterm:"less_than(0,0)"
     } :< Json.

test(quickcheck_uses_test_count_setting,
     [ setup(setting(quickcheck:test_count, Old)),
       cleanup(set_setting(quickcheck:test_count, Old))
     ]) :-
    set_setting(quickcheck:test_count, 7),
    run_quickcheck(qc_props:reverse_twice/1, Json),
    _{testcount:7} :< Json.

test(quickcheck_missing_property_throws,
     throws(error(existence_error(predicate, qc_props:missing/1), _))) :-
    quickcheck(qc_props:missing/1).

test(quickcheck_wrong_arity_throws,
     throws(error(existence_error(predicate, qc_props:reverse_twice/3), _))) :-
    quickcheck(qc_props:reverse_twice/3).

:- end_tests(quickcheck).
