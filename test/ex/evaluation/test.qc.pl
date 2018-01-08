
rolo(List, Rev) :-
        rolo(List, Rev, []).

rolo([], L, L).
rolo([H|T], L, SoFar) :-
        rolo(T, L, [5|SoFar]).

prop_reverse_twice(L:list(integer)) :-
    rolo(L, R),
    rolo(R, L).


prop_reverse_quad(L:list(integer)) :-
    rolo(A, B),
    rolo(B, C),
    rolo(C, D),
    rolo(D, A).


lol(jan,piet).
lol(A,B) :- lol(B,A).

prop_infinite(L:list(integer)) :-
    lol(A, A).
