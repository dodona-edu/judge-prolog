% hug(X,Y) :- hug(Y,X).

brohug(X,Y) :- hug(X,Y) , male(X), male(Y), justfriends(X,Y).
realhug(X,Y) :- hug(X,Y), hug(Y,X).