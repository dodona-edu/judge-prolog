%- consult database.pl
:- module(dodonaevaluate, [tests/5]).

correct(3,4).
correct(3,panda(_)).

tests(dodonaevaluate:correct,student,true,10000,[[0,0],[3,panda(4)],[1,4],[5,2]]).