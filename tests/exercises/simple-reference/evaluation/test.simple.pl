:- module(dodonaevaluate, [tests/6]).

checked_double(X, _) :- X < 0, throw(negative(X)).
checked_double(X, Y) :- X >= 0, Y is 2 * X.

digit(D) :- between(0, 9, D).

countdown(N) :- N > 0, M is N - 1, countdown(M).
countdown(0).

first([X|_], X).

tests("Exceptions", dodonaevaluate:checked_double, checked_double, false, 1000, [[2, _], [-1, _]]).
tests("Choice points", dodonaevaluate:digit, digit, true, 1000, [[_], [5], [10]]).
tests("Inference limit", dodonaevaluate:countdown, countdown, false, 1000, [[10], [100000]]).
tests("Shared variables", dodonaevaluate:first, first, false, 1000, [[[a, b], X], [[X, c], _]]).
