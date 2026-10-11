realhug(X, Y) :- hug(X, Y).

brohug(X, Y) :- hug(X, Y), male(X), male(Y), justfriends(Y, X).
