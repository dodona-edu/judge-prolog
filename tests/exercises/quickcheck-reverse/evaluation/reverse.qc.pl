prop_reverse_twice(L:list(integer)) :-
    my_reverse(L, R),
    my_reverse(R, L).

prop_reverse_length(L:list(integer)) :-
    % A reversed list is as long as the original.
    my_reverse(L, R),
    length(L, N),
    length(R, N).

prop_reverse_append(A:list(integer), B:list(integer)) :-
    append(A, B, AB),
    my_reverse(AB, R),
    my_reverse(A, RA),
    my_reverse(B, RB),
    append(RB, RA, R).
