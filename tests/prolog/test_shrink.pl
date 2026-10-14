:- use_module('../../quicktest/quickcheck').
:- use_module(library(plunit)).

:- begin_tests(shrink).

candidates(Type, Value, Candidates) :-
    findall(X, shrink(Type, Value, X), Candidates).

integer_value(-12345).
integer_value(-100).
integer_value(-7).
integer_value(-1).
integer_value(0).
integer_value(1).
integer_value(2).
integer_value(3).
integer_value(100).
integer_value(12345).

test(integer_zero_is_first_candidate, [forall((integer_value(X), X =\= 0))]) :-
    once(shrink(integer, X, 0)).

test(integer_candidates_are_no_larger, [forall(integer_value(X))]) :-
    candidates(integer, X, Candidates),
    forall(member(Y, Candidates), abs(Y) =< abs(X)).

test(positive_integer_bisects_towards_zero) :-
    candidates(integer, 100, [0, 1, 3, 7, 20, 55]).

test(negative_integer_tries_its_positive_version) :-
    candidates(integer, -7, [0, 7]).

test(integers_do_not_shrink_to_themselves,
     [fixme('#31: yields 0 and 1 for themselves')]) :-
    \+ shrink(integer, 0, _),
    candidates(integer, 1, [0]).

test(code_candidates_are_fixed_characters) :-
    candidates(code, 0'z, [0'a, 0'b, 0'c, 0' ]).

test(list_candidates_are_sublists_or_shrunk_elements) :-
    L0 = [5, -6, 7, 8, 9],
    forall(shrink(list(integer), L0, L), sublist_or_smaller(L0, L)).

test(list_shrinks_to_empty_list) :-
    once(shrink(list(integer), [3, 4, 5], [])).

test(list_removes_pieces_of_the_list) :-
    candidates(list(integer), [1, 2, 3, 4], Candidates),
    memberchk([], Candidates),
    memberchk([4], Candidates),
    memberchk([2, 3, 4], Candidates),
    memberchk([1, 2, 4], Candidates).

test(list_shrinks_single_elements) :-
    candidates(list(integer), [4, 9], Candidates),
    memberchk([0, 9], Candidates),
    memberchk([4, 0], Candidates).

test(empty_list_only_shrinks_to_itself) :-
    candidates(list(integer), [], [[]]).

test(untyped_list_shrinks_like_list_of_any) :-
    candidates(list, [a, b], Candidates),
    candidates(list(any), [a, b], Candidates).

test(any_is_not_shrunk) :-
    candidates(any, foo, [foo]).

test(atom_candidates_are_no_longer, [forall(member(A, [abc, a, '']))]) :-
    atom_length(A, Len),
    forall(shrink(atom, A, S),
           ( atom(S),
             atom_length(S, SLen),
             SLen =< Len
           )).

test(atom_shrinks_to_empty_atom) :-
    once(shrink(atom, hello, '')).

test(codes_shrink_like_lists_of_codes) :-
    candidates(codes, [0'a, 0'b], Codes),
    candidates(list(code), [0'a, 0'b], Codes).

test(string_candidates_come_from_the_string) :-
    string_codes("abc", Codes),
    candidates(codes, Codes, CodeCandidates),
    maplist(string_codes, Expected, CodeCandidates),
    candidates(string, "abc", Expected).

test(unsupported_type_has_no_candidates, fail) :-
    shrink(no_such_type, 1, _).

test(types_without_shrinker_have_no_candidates,
     [forall(member(T-V, [boolean-true, float-1.5, between(1, 5)-3]))]) :-
    \+ shrink(T, V, _).

test(repeated_shrinking_terminates) :-
    Max = 64,
    shrink_chain(list(integer), [100, -200, 300, 400, 500, 600, 700, 800], Max, Steps),
    Steps < Max.

sublist_or_smaller(L0, L) :-
    (   subseq(L0, L, _)
    ->  true
    ;   same_length(L0, L),
        maplist(no_larger, L0, L)
    ).

no_larger(X, Y) :-
    abs(Y) =< abs(X).

shrink_chain(Type, Value, Max, Steps) :-
    shrink_chain(Type, Value, Max, 0, Steps).

shrink_chain(Type, Value, Max, Steps0, Steps) :-
    Steps0 < Max,
    shrink(Type, Value, Next),
    Next \== Value,
    !,
    Steps1 is Steps0 + 1,
    shrink_chain(Type, Next, Max, Steps1, Steps).
shrink_chain(_, _, _, Steps, Steps).

:- end_tests(shrink).
