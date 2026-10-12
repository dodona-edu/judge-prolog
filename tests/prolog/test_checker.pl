:- use_module('../../simpletest/checker').
:- use_module(library(plunit)).
:- use_module(support).

:- dynamic dodonaevaluate:tests/6.

% Reference implementations live in dodonaevaluate, submissions in user,
% like the judge loads them.

dodonaevaluate:double(X, Y) :-
    Y is X * 2.

double(X, Y) :-
    Y is X * 2.

wrong_double(X, Y) :-
    Y is X + 2.

failing_double(_, _) :-
    fail.

member_twice(X, Y) :-
    member(Y, [X, X]).

dodonaevaluate:member_twice(X, Y) :-
    member(Y, [X, X]).

choice_double(X, Y) :-
    member(_, [a, b]),
    Y is X * 2.

looping(_, _) :-
    looping(_, _).

dodonaevaluate:looping(_, _) :-
    dodonaevaluate:looping(_, _).

throwing(_, _) :-
    throw(my_exception).

type_error_double(X, _) :-
    atom_length(X, _),
    X = f(_).

printing_double(X, Y) :-
    format("hello~n"),
    Y is X * 2.

:- begin_tests(checker, [cleanup(retractall(dodonacheck:test_result(_)))]).

check(Tests, Results) :-
    check(Tests, Results, _).

check(Tests, Results, Output) :-
    retractall(dodonacheck:test_result(_)),
    setup_call_cleanup(
        forall(member(T, Tests), assertz(dodonaevaluate:T)),
        with_temp_dir(( with_output_to(string(Output), dodonacheck:dotests()),
                        read_result(Results)
                      )),
        retractall(dodonaevaluate:tests(_, _, _, _, _, _))).

cases(Json, Cases) :-
    [Test] = Json,
    get_dict(result, Test, Cases).

summaries(Json, Summaries) :-
    cases(Json, Cases),
    reverse(Cases, InOrder),
    findall(Expected-Got,
            ( member(Case, InOrder),
              get_dict(expected, Case, Expected),
              get_dict(got, Case, Got)
            ),
            Summaries).

terms(Json, Terms) :-
    cases(Json, Cases),
    findall(Term, ( member(Case, Cases), get_dict(term, Case, Term) ), Terms).

test(matching_results_are_equal) :-
    check([tests("double", dodonaevaluate:double, double, true, 1000, [[1, _], [2, _]])], Json),
    summaries(Json, [E1-G1, E2-G2]),
    E1 == G1,
    E2 == G2.

test(deterministic_success_is_exit) :-
    check([tests("double", dodonaevaluate:double, double, true, 1000, [[1, _]])], Json),
    summaries(Json, ["exit"-"exit"]).

test(wrong_answer_fails_with_the_reference_result_bound) :-
    check([tests("double", dodonaevaluate:double, wrong_double, true, 1000, [[1, _]])], Json),
    summaries(Json, ["exit"-"fail"]),
    terms(Json, ["wrong_double(1,2)"]).

test(failure_is_reported) :-
    check([tests("double", dodonaevaluate:double, failing_double, true, 1000, [[1, _]])], Json),
    summaries(Json, ["exit"-"fail"]).

test(remaining_choice_point_is_reported) :-
    check([tests("member", dodonaevaluate:member_twice, member_twice, true, 1000, [[1, _]])], Json),
    summaries(Json, ["true"-"true"]).

test(student_choice_point_differs_from_reference) :-
    check([tests("choice", dodonaevaluate:double, choice_double, true, 1000, [[1, _]])], Json),
    summaries(Json, ["exit"-"true"]).

test(inference_limit_is_reported) :-
    check([tests("loop", dodonaevaluate:double, looping, true, 1000, [[1, _]])], Json),
    summaries(Json, ["exit"-"inference_limit_exceeded"]).

test(inference_limit_applies_to_the_reference) :-
    check([tests("loop", dodonaevaluate:looping, looping, true, 1000, [[1, _]])], Json),
    summaries(Json, ["inference_limit_exceeded"-"inference_limit_exceeded"]).

test(ground_exception_is_reported_as_term) :-
    check([tests("throw", dodonaevaluate:double, throwing, true, 1000, [[1, _]])], Json),
    summaries(Json, ["exit"-"my_exception"]).

test(error_is_reported_with_context) :-
    check([tests("error", dodonaevaluate:double, type_error_double, true, 1000, [[_, _]])], Json),
    summaries(Json, [_-Got]),
    sub_string(Got, 0, _, _, "instantiation_error in context(system:atom_length/2").

test(error_without_context_is_reported_as_term) :-
    check([tests("error", dodonaevaluate:double, double, true, 1000, [[a, _]])], Json),
    summaries(Json, [_-Got]),
    sub_string(Got, 0, _, _, "type_error(evaluable,a/0) in ").

test(term_shows_the_student_call) :-
    check([tests("double", dodonaevaluate:double, double, true, 1000, [[21, _]])], Json),
    terms(Json, ["double(21,42)"]).

test(results_are_listed_last_case_first) :-
    check([tests("double", dodonaevaluate:double, double, true, 1000, [[1, _], [2, _]])], Json),
    terms(Json, ["double(2,4)", "double(1,2)"]).

test(test_settings_are_copied) :-
    check([tests("my name", dodonaevaluate:double, double, false, 1234, [[1, _]])], Json),
    [Test] = Json,
    _{name:"my name", allowcp:"false", inferencelimit:1234} :< Test.

test(each_test_gets_its_own_result) :-
    check([ tests("one", dodonaevaluate:double, double, true, 1000, [[1, _]]),
            tests("two", dodonaevaluate:double, wrong_double, true, 1000, [[2, _]])
          ],
          Json),
    [One, Two] = Json,
    _{name:"one"} :< One,
    _{name:"two"} :< Two.

test(no_tests_gives_empty_result) :-
    check([], []).

test(no_cases_gives_empty_case_list) :-
    check([tests("empty", dodonaevaluate:double, double, true, 1000, [])], Json),
    cases(Json, []).

test(output_of_the_predicates_goes_to_standard_output) :-
    check([tests("print", dodonaevaluate:double, printing_double, true, 1000, [[1, _]])], Json, Output),
    Output == "hello\n",
    summaries(Json, ["exit"-"exit"]).

:- end_tests(checker).
