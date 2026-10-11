parent(ann, bob).
parent(bob, cid).

:- begin_tests(grandparent).
test(ann_cid) :-
    assertion(grandparent(ann, cid)).
:- end_tests(grandparent).
