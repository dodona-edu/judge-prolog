:- module(dodonaevaluate, [tests/5]).

correct(3,4).
correct(3,panda(_)).

tests(dodonaevaluate:correct,stuff,true,10000,[[0,0],[3,panda(4)],[1,4],[2,5],[5,2]]).