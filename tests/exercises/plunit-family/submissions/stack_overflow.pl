realhug(X, Y) :- realhug(Y, X), hug(X, Y).

brohug(X, Y) :- hug(X, Y), male(X), male(Y), justfriends(X, Y).
