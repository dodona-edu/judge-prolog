% At the moment the stderr is 
%
% Warning: /tmp/b.pl:3:
%         Singleton variables: [X]
% Warning: /tmp/b.pl:10:
%         Clauses of k/1 are not together in the source-file
%           Earlier definition at /tmp/b.pl:5
%           Current predicate: g/1
%           Use :- discontiguous k/1. to suppress this message
%
% And the output json is:
%
% [
%   {
%     "msg": "fwfre/0, which is referenced by\n\t/tmp/b.pl:3:10: 1-st clause of lol/1",
%     "type": "undefined"
%   },
%   {
%     "msg": "k(2), which is called from\n\t/tmp/b.pl:8:8: 2-nd clause of g/1",
%     "type": "trivial_failure"
%   },
%   {
%     "msg": "k(3), which is called from\n\t/tmp/b.pl:7:8: 1-st clause of g/1",
%     "type": "trivial_failure"
%   }
% ]
% 
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