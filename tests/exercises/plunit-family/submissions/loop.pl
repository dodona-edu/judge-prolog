realhug(X, Y) :- hug(X, Y), hug(Y, X).

brohug(_, _) :- repeat, fail.
