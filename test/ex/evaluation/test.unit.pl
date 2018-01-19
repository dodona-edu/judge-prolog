:- consult(database).


:- begin_tests(realh).
    test(realhreg) :-
        assertion(realhug(jan,fien)).
:- end_tests(realh).

:- begin_tests(realh1).
    test(realhreg) :-
        assertion(realhug(jan,fien)).
:- end_tests(realh1).

:- begin_tests(hugging).
    test(brohug) :-
        assertion(brohug(jan,tom)).

    test(nobrohug) :-
        assertion(brohug(tom,jan)),
        assertion(brohug(jan,fien)).
:- end_tests(hugging).

:- begin_tests(noname).
    test(brohug) :-
        assertion(lol(jan,tom)),
        assertion(lol(tom,jan)),
        assertion(lol(jan,fien)).
    test(realhreg) :-
        assertion(realhug(jan,fien)).
:- end_tests(noname).
