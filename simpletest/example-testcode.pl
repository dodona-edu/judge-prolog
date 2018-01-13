
:- set_prolog_flag(readline,false).
:- use_module("checker.pl").
:- consult("submission.pl").
:- load_files("evaluate.pl",[module(dodonaevaluate)]).

