my_reverse(L, R) :- my_reverse(L, [], R).

my_reverse([], R, R).
my_reverse([H|T], Acc, R) :- my_reverse(T, [H|Acc], R).
