:- consult(database).

:- begin_tests(realhug).
%! realhug/2 holds when two people hug each other.
test(mutual) :-
    assertion(realhug(jan, fien)).

test(one_sided, [fail]) :-
    realhug(tom, fien).
:- end_tests(realhug).

:- begin_tests(brohug).
test(friends) :-
    assertion(brohug(jan, tom)).

test(not_friends, [fail]) :-
    brohug(jan, fien).
:- end_tests(brohug).

:- begin_tests(all_realhugs).
test(all, [set(X-Y == [fien-jan, jan-fien, jan-tom, tom-jan])]) :-
    realhug(X, Y).
:- end_tests(all_realhugs).
