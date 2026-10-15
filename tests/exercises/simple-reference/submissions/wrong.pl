checked_double(X, Y) :- Y is 2 * X.

digit(D) :- member(D, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

countdown(N) :- countdown(N).

first([_, X|_], X).
