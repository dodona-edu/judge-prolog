checked_double(X, _) :- X < 0, throw(negative(X)).
checked_double(X, Y) :- X >= 0, Y is 2 * X.

digit(D) :- between(0, 9, D).

countdown(N) :- N > 0, M is N - 1, countdown(M).
countdown(0).

first([X|_], X).
