:- module(dodonaevaluate, [tests/6]).

is_left_of(waldo,odlaw).
is_left_of(odlaw,swi).
is_left_of(swi,wilma).
is_left_of(whitebeard,woof).
is_left_of(woof,watchers).

is_below_of(whitebeard,waldo).
is_below_of(woof,odlaw).
is_below_of(watchers,swi).

is_right_of(X,Y) :- is_left_of(Y,X).
is_above_of(X,Y) :- is_below_of(Y,X).

human(waldo).
human(odlaw).
human(swi).
human(wilma).
human(whitebeard).
human(woof).
human(watchers).

tests("`is_left_of`",dodonaevaluate:is_left_of,is_left_of,true,10000, L) :-
    setof([X,Y],(human(X),human(Y)),L).
tests("`is_below_of`",dodonaevaluate:is_below_of,is_below_of,true,10000, L) :-
    setof([X,Y],(human(X),human(Y)),L).
tests("`is_right_of`",dodonaevaluate:is_right_of,is_right_of,true,10000, L) :-
    setof([X,Y],(human(X),human(Y)),L).
tests("`is_above_of`",dodonaevaluate:is_above_of,is_above_of,true,10000, L) :-
    setof([X,Y],(human(X),human(Y)),L).
