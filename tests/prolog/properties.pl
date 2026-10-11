:- module(qc_props,
          [ reverse_twice/1,
            small_integer/1,
            less_than/2,
            lossy_reverse_keeps_length/1,
            throws_error/1,
            throws_term/1,
            unsupported/1,
            always_fails/1
          ]).

% Properties as quickcheck expects them: one Name:Type argument per type.

reverse_twice(X:list(integer)) :-
    reverse(X, Y),
    reverse(Y, X).

small_integer(X:integer) :-
    X < 100.

less_than(X:integer, Y:integer) :-
    X < Y.

lossy_reverse_keeps_length(X:list(integer)) :-
    lossy_reverse(X, Y),
    length(X, N),
    length(Y, N).

lossy_reverse([], []).
lossy_reverse([H|T], R) :-
    reverse([H|T], [_|R]).

throws_error(_:integer) :-
    atom_length(_, _).

throws_term(_:integer) :-
    throw(my_exception).

unsupported(_:no_such_type).

always_fails(_:integer) :-
    fail.
