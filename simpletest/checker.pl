:- module(dodonacheck,
          [ dotests/0
          ]).

% I don't realy know why I've put these here anymore
:- set_prolog_flag(unknown, error). 
:- set_prolog_flag(report_error, true).
% Print a one line trace in case an error occurs
% (More messes with the inference limit)
:- use_module(library(prolog_stack)).
:- use_module(library(error)).

user:prolog_exception_hook(Exception, Exception, Frame, _) :-
    (   Exception=error(Term)
    ;   Exception=error(Term, _)
    ),
    get_prolog_backtrace(Frame, 1, Trace),
    format(user_error, 'ERROR: ~p.', [Term]),
    nl(user_error),
    print_prolog_backtrace(user_error, Trace),
    nl(user_error),
    true. %don't stop at errors


% The exported term dotests evaluates all tests in the dodonaevaluate module
% Todo, allow for multiple statements
dotests() :-
    findall([Name, ImplOk,ImplStud,AllowCP,Limit,Cases],dodonaevaluate:tests(Name,ImplOk,ImplStud,AllowCP,Limit,Cases),Tests),
    dotests(Tests).


% When everyting is parsed the testresults are the results.
% All tets are executed and the result is stored in a test_result(Res) using asertz/1.
% This is needed because backtracking out of setup_call_catcher_cleanup/4 destroys
% all choisepoints taken in the catcher.
dotests([]) :-
    findall(R,test_result(R), Res),
    open('result.json', write, Stream),
    json_write(Stream,Res),
    close(Stream).


dotests([[Name, ImplOk,ImplStud,AllowCP,Limit,Cases]|Xs]) :-
    verified(Name, ImplOk,ImplStud,AllowCP,Limit,Cases),
    dotests(Xs). 


% Start with an empty result list
verified(Name, ImplOk, ImplStud, AllowCP,Limit, List) :-
    verified(Name, ImplOk, ImplStud, AllowCP,Limit, List, []).

% When all cases are handled write the result to a file
:- use_module(library(http/json)).
verified(Name, _, _, AllowCP,Limit, [], Result) :-
    assertz(test_result(res{name: Name,allowcp:AllowCP, inferencelimit:Limit, result:Result})).

% Checks what the result is of the correct code
verified(Name, ImplOk, ImplStud, AllowCP,Limit, [Arg|RemArgs], Result) :-
    ignore(catch(setup_call_catcher_cleanup(true,
        call_with_inference_limit(
                catch(apply(ImplOk,Arg),Exception,true),
                Limit,
                LimitResult),
        Caught,
        (
            mkresult(Caught, LimitResult,Exception, Smry), 
            verified_student(Smry, Name, ImplOk, ImplStud, AllowCP,Limit, Arg, RemArgs, Result)
        )),_,true)).

% checks the result of the students code
% Extra catches required
verified_student(Expected, Name, ImplOk, ImplStud, AllowCP,Limit, Arg, RemArgs, Result) :-
    ignore(setup_call_catcher_cleanup(true,
            call_with_inference_limit(
                catch(apply(ImplStud,Arg),Exception,true),
                Limit,
                LimitResult),
        Caught,
        (
            H=..[ImplStud|Arg], swritef(C, '%q', [H]), 
            mkresult(Caught, LimitResult, Exception, Smry), 
            verified(Name, ImplOk, ImplStud, AllowCP,Limit, RemArgs, 
                [res{expected:Expected, got:Smry, term:C}|Result])
        ))).


% Format a testresult that is JSON stringifiable


mkresult(_, _,error(Exception,Context),Summary) :- ground(Exception),!,
    swritef(Summary, '%q in %q', [Exception,Context]).

mkresult(_, _,error(Exception),Summary) :- ground(Exception),!,
    swritef(Summary, '%q', [Exception]).

mkresult(_, _, Exception, Summary) :- ground(Exception),!,
    swritef(Summary, '%q', [Exception]).


mkresult(Caught, LimitResult,_,Summary) :- !,
    mkresult(Caught, LimitResult,Summary).

mkresult(exception(Exception),_,Summary) :- !,
    swritef(Summary, '%q', [Exception]). 

mkresult(external_exception(Exception),_,Summary) :- !,
    swritef(Summary, '%q', [Exception]). 

mkresult(fail,_,Summary) :- !,
    swritef(Summary, '%q', [fail]). 

mkresult(exit,!,Summary) :- !,
    swritef(Summary, '%q', [exit]). 

mkresult(!,true,Summary) :- !,
    swritef(Summary, '%q', [true]). 

mkresult(_,inference_limit_exceeded, Summary) :- !,
    swritef(Summary, '%q', [inference_limit_exceeded]). 

mkresult(Catched, LimitResult, Summary) :-
    (   ground(LimitResult)
    ->  swritef(LimitStr, '%q', [LimitResult])
    ;   LimitStr=null
    ),
    swritef(CatchedStr, '%q', [Catched]),
    Summary=testres{limitresult:LimitStr, returncode:CatchedStr}.