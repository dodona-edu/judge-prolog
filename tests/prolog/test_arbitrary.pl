:- use_module('../../quicktest/quickcheck').
:- use_module(library(plunit)).

draws(100).

:- begin_tests(arbitrary, [setup(set_random(seed(1)))]).

simple_type(atom).
simple_type(atomic).
simple_type(boolean).
simple_type(chars).
simple_type(code).
simple_type(codes).
simple_type(encoding).
simple_type(float).
simple_type(integer).
simple_type(list).
simple_type(nonneg).
simple_type(number).
simple_type(positive_integer).
simple_type(rational).
simple_type(string).
simple_type(text).

all_draws_are(Type, Check) :-
    draws(N),
    forall(between(1, N, _),
           ( arbitrary(Type, X),
             call(Check, X)
           )).

sample(N, Type, Xs) :-
    findall(X, (between(1, N, _), arbitrary(Type, X)), Xs).

has_type(Type, X) :-
    must_be(Type, X).

test(value_has_type, [forall(simple_type(Type))]) :-
    all_draws_are(Type, has_type(Type)).

test(between_integers_stay_in_bounds) :-
    all_draws_are(between(-3, 3), has_type(between(-3, 3))).

test(between_integers_reach_both_bounds) :-
    sample(200, between(1, 3), Xs),
    sort(Xs, [1, 2, 3]).

test(between_floats_stay_in_bounds) :-
    all_draws_are(between(0.5, 1.5), has_type(between(0.5, 1.5))).

test(oneof_draws_from_list) :-
    all_draws_are(oneof([a, b, c]), memberchk_in([a, b, c])).

test(oneof_reaches_every_element) :-
    sample(100, oneof([a, b, c]), Xs),
    sort(Xs, [a, b, c]).

test(oneof_empty_list_fails, fail) :-
    arbitrary(oneof([]), _).

test(list_of_integers) :-
    all_draws_are(list(integer), has_type(list(integer))).

test(nested_list) :-
    all_draws_are(list(list(boolean)), has_type(list(list(boolean)))).

test(list_of_oneof) :-
    all_draws_are(list(oneof([x, y])), has_type(list(oneof([x, y])))).

test(list_is_never_empty) :-
    all_draws_are(list(integer), nonempty).

test(list_has_at_most_thirty_elements) :-
    all_draws_are(list(integer), at_most_thirty).

test(code_is_printable_ascii) :-
    all_draws_are(code, printable_ascii).

test(codes_are_printable_ascii) :-
    all_draws_are(codes, maplist(printable_ascii)).

test(positive_integer_is_positive) :-
    all_draws_are(positive_integer, positive).

test(nonneg_is_not_negative) :-
    all_draws_are(nonneg, non_negative).

test(any_draws_a_supported_value) :-
    all_draws_are(any, ground).

test(unsupported_type_fails, fail) :-
    arbitrary(no_such_type, _).

test(same_seed_gives_same_values) :-
    set_random(seed(7)),
    sample(10, list(integer), Xs),
    set_random(seed(7)),
    sample(10, list(integer), Xs).

test(negative_integer_is_at_most_one) :-
    all_draws_are(negative_integer, integer_at_most_one).

test(negative_integer_is_negative) :-
    % With seed 1, 0 or 1 is drawn within the first 100 draws.
    set_random(seed(1)),
    all_draws_are(negative_integer, negative).

test(rational_draws_never_throw) :-
    % With seed 3 the first zero denominator is draw 64443.
    set_random(seed(3)),
    forall(between(1, 65000, _),
           ( arbitrary(rational, X),
             must_be(rational, X)
           )).

test(arbitrary_type_lists_supported_types) :-
    forall(simple_type(Type), arbitrary_type(Type)),
    arbitrary_type(list(_)),
    arbitrary_type(between(_, _)),
    arbitrary_type(oneof(_)).

test(arbitrary_type_rejects_unsupported_type, fail) :-
    arbitrary_type(no_such_type).

test(arbitrary_type_is_a_type_for_must_be) :-
    must_be(arbitrary_type, integer),
    is_of_type(arbitrary_type, list(integer)),
    \+ is_of_type(arbitrary_type, no_such_type).

memberchk_in(List, X) :-
    memberchk(X, List).

nonempty([_|_]).

at_most_thirty(L) :-
    length(L, N),
    N =< 30.

printable_ascii(C) :-
    between(0x20, 0x7e, C).

positive(X) :-
    X >= 1.

negative(X) :-
    X < 0.

non_negative(X) :-
    X >= 0.

integer_at_most_one(X) :-
    integer(X),
    X =< 1.

:- end_tests(arbitrary).
