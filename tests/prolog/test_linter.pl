:- use_module(library(plunit)).
:- use_module(support).

% Runs the linter like formcheck.py does and returns the reported
% messages (result.json) and the warnings printed on stderr.

:- dynamic linter_file/1.

:- prolog_load_context(directory, Dir),
   absolute_file_name('../../linter/linter.pl', Path, [relative_to(Dir)]),
   assertz(linter_file(Path)).

lint(Submission, Messages, Stderr) :-
    lint([], Submission, Messages, Stderr).

lint(Predefined, Submission, Messages, Stderr) :-
    linter_file(Linter),
    with_temp_dir(( write_files(Predefined, Linter, Submission),
                    run_swipl('\'dodona lint\'', 'driver.pl', _, Stderr),
                    read_result(Messages)
                  )).

write_files(Predefined, Linter, Submission) :-
    write_file('submission.pl', Submission),
    setup_call_cleanup(
        open('driver.pl', write, D),
        ( forall(member(P, Predefined), format(D, ":- dynamic ~w.~n", [P])),
          format(D, ":- use_module(library(check)).~n", []),
          format(D, ":- include('~w').~n", [Linter]),
          format(D, ":- consult('submission.pl').~n", [])
        ),
        close(D)).

contains(String, Part) :-
    sub_string(String, _, _, _, Part),
    !.

messages_of_type(Messages, Type, Matching) :-
    findall(Text,
            ( member(M, Messages),
              get_dict(type, M, Type),
              get_dict(msg, M, Text)
            ),
            Matching).

:- begin_tests(linter).

test(correct_submission_has_no_messages) :-
    lint("parent(a, b).\nancestor(X, Y) :- parent(X, Y).\n", [], "").

test(undefined_predicate_is_reported) :-
    lint("lol(X) :- fwfre(X).\n", Messages, _),
    messages_of_type(Messages, "undefined", [Text]),
    contains(Text, "fwfre/1, which is referenced by"),
    contains(Text, "1-st clause of lol/1").

test(every_undefined_predicate_is_listed) :-
    lint("a :- b.\nc :- d.\n", Messages, _),
    messages_of_type(Messages, "undefined", [Text]),
    contains(Text, "b/0"),
    contains(Text, "d/0").

test(predefined_dynamic_predicate_is_not_undefined) :-
    lint(["fact/1"], "lol(X) :- fact(X).\n", [], _).

test(call_without_matching_clause_is_a_trivial_failure) :-
    lint("g(1) :- k(3).\nk(1).\n", Messages, _),
    messages_of_type(Messages, "trivial_failure", [Text]),
    contains(Text, "k(3), which is called from"),
    contains(Text, "1-st clause of g/1").

test(call_with_matching_clause_is_not_a_trivial_failure) :-
    lint("g(1) :- k(1).\nk(1).\n", [], _).

test(syntax_error_is_reported_as_error) :-
    lint("p :- foo(.\nq.\n", Messages, _),
    messages_of_type(Messages, "error", [Text]),
    contains(Text, "submission.pl:1:"),
    contains(Text, "Syntax error").

test(syntax_error_is_not_printed_on_stderr) :-
    lint("p :- foo(.\nq.\n", _, Stderr),
    \+ contains(Stderr, "Syntax error").

test(messages_keep_the_order_error_undefined_trivial_failure) :-
    lint("p :- foo(.\nlol(X) :- fwfre(X).\ng(1) :- k(3).\nk(1).\n", Messages, _),
    findall(T, ( member(M, Messages), get_dict(type, M, T) ), Types),
    Types == ["error", "undefined", "trivial_failure"].

% The linter only reports these; formcheck.py reads them from stderr.
test(singleton_variable_stays_on_stderr) :-
    lint("p(X, Y) :- q(X).\nq(1).\n", Messages, Stderr),
    Messages == [],
    contains(Stderr, "Singleton variables: [Y]").

test(discontiguous_clauses_stay_on_stderr) :-
    lint("a(1).\nb(1).\na(2).\n", Messages, Stderr),
    Messages == [],
    contains(Stderr, "Clauses of a/1 are not together").

:- end_tests(linter).
