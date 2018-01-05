
rolo(List, Rev) :-
        rolo(List, Rev, []).

rolo([], L, L).
rolo([H|T], L, SoFar) :-
        rolo(T, L, [H|SoFar]).

prop_reverse_twice(L:list(int,eg(s),er),R:trol) :-
    rolo(L, R),
    rolo(R, L).


prop_reverse_quad(L:list(integer)) :-
    rolo(A, B),
    rolo(B, C),
    rolo(C, D),
    rolo(D, A).
