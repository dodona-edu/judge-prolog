parent(ann, bob).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
