:- use_module(library(http/json)).
:- dynamic cres/1.

message_hook(M, _, _):-
    M=check(undefined_procedures, _),
    message_to_string(M,S),
    assertz(cres(point{type:undefined,msg:S})),
    true.

message_hook(M, _, _):-
    M=check(X),functor(X,trivial_failure,_),
    message_to_string(M,S),
    assertz(cres(point{type:trivial_failure,msg:S})),
    true.

message_hook(M,_,_):-
    functor(M,error,_),
    message_to_string(M,S),
    assertz(cres(point{type:error,msg:S})),
    true.


'dodona lint' :- !,
    check:check,
    findall(R,cres(R), Res),
    open('result.json', write, Stream),
    json_write(Stream,Res),
    close(Stream).