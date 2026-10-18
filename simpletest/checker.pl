:- module(dodonacheck,
          [ dotests/0
          ]).

% Print a one line trace in case an error occurs
% (More messes with the inference limit)
:- use_module(library(prolog_stack)).
:- use_module(library(http/json)).

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


% Runs every test in the dodonaevaluate module and writes the results to result.json.
dotests :-
    findall(test(Name, ImplOk, ImplStud, AllowCP, Limit, Cases),
            dodonaevaluate:tests(Name, ImplOk, ImplStud, AllowCP, Limit, Cases),
            Tests),
    convlist(test_result, Tests, Results),
    setup_call_cleanup(
        open('result.json', write, Stream),
        json_write(Stream, Results),
        close(Stream)).

% A test that raises an exception outside the predicates it calls is left out.
test_result(test(Name, ImplOk, ImplStud, AllowCP, Limit, Cases), Result) :-
    catch(maplist(case_result(ImplOk, ImplStud, Limit), Cases, CaseResults), _, fail),
    % Last case first, as before: simpltest.py's sampling of shown cases depends on the order.
    reverse(CaseResults, Reversed),
    Result = res{name:Name, allowcp:AllowCP, inferencelimit:Limit, result:Reversed}.

% Bindings of a predicate that succeeds stay: the student predicate is called
% with the answer of the reference solution, and later cases see both.
case_result(ImplOk, ImplStud, Limit, Arg, res{expected:Expected, got:Got, term:Term}) :-
    run(ImplOk, Arg, Limit, Expected),
    run(ImplStud, Arg, Limit, Got),
    Goal =.. [ImplStud|Arg],
    swritef(Term, '%q', [Goal]).

% Errors of Impl, inference_limit_exceeded included, end up in Error.
run(Impl, Arg, Limit, Summary) :-
    outcome(catch(apply(Impl, Arg), Error, true), Limit, Outcome),
    summary(Outcome, Error, Summary).

% Outcome is the result of call_with_inference_limit/3, fail or exception(E).
% Cutting a remaining choice point can run a cleanup handler that throws, hence the catch around ->.
outcome(Goal, Limit, Outcome) :-
    catch(( call_with_inference_limit(Goal, Limit, Result)
          ->  Outcome = Result
          ;   Outcome = fail
          ),
          E,
          Outcome = exception(E)).

% Format a testresult that is JSON stringifiable
summary(_, error(Exception, Context), Summary) :- ground(Exception), !,
    swritef(Summary, '%q in %q', [Exception, Context]).
summary(_, error(Exception), Summary) :- ground(Exception), !,
    swritef(Summary, '%q', [Exception]).
summary(_, Exception, Summary) :- ground(Exception), !,
    swritef(Summary, '%q', [Exception]).
summary(exception(Exception), _, Summary) :- !,
    swritef(Summary, '%q', [Exception]).
summary(Outcome, _, Summary) :-
    port(Outcome, Port),
    swritef(Summary, '%q', [Port]).

port(!, exit).
port(true, true).
port(fail, fail).
port(inference_limit_exceeded, inference_limit_exceeded).
