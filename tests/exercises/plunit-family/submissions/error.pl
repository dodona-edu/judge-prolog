realhug(X, Y) :- hug(X, Y), Z is X + Y, Z > 0.

brohug(X, Y) :- hug(X, Y), male(X), male(Y), justfriends(X, Y).
