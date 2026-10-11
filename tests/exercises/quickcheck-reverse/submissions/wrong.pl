my_reverse([], []).
my_reverse([_|T], R) :- my_reverse(T, R).
