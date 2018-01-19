%- consult public "database.pl".
:- module(dodonaevaluate, [tests/6]).

correct(3,4).
correct(3,panda(_)).


human(X) :- user:male(X).
human(X) :- user:female(X).


tests("demo",dodonaevaluate:correct,stuff,true,10000,[[0,0],[3,panda(4)],[1,4],[2,5],[5,2]]) :- human(jan).