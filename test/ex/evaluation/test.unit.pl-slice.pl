
:- consult("/home/beardhatcode/Documents/dodona/dodona/data/judges/judge-prolog.git/test/ex/solution/input.pl").
:- consult(database).
:- begin_tests(noname).
    test(brohug) :-
        assertion(lol(jan,tom)),
        assertion(lol(tom,jan)),
        assertion(lol(jan,fien)).
    test(realhreg) :-
        assertion(realhug(jan,fien)).
:- end_tests(noname).
