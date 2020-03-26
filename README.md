# judge-prolog

A judge for the Prolog language.

## Format of the QuickCheck tests

QuickCheck testfiles should be named with a name ending in `.qc.pl`. The name
before the extension will be used as tabname.

All tests must have a name starting with `prop_`. There should be at least one
blank line separating the definition from other things. A property must not
contain empty lines in its definition (you may use comments to fill the space).
Tests may contain comments. Comments within the test will be shown to the
student, comments outside it wil not.

```prolog
prop_reverse_twice(L:list(integer)) :-
    % This comment is shown to students
    my_reverse(L, R),
    my_reverse(R, L).
```

## Format of PLUnit testfiles

PLUnit testfiles should be named with a name ending in `.unit.pl`. The name
before the extension will be used as tabname.

The testfile will be split up into the parts between the tags
`:- begin_tests(...).` and `:- end_tests(...).`. All the lines specified before
the `begin_tests` and not in another part can be used.

```prolog
:- consult(database).

:- begin_tests(groupname).
test(name_of_the_test) :- clause(largest(A,B,A),true).
test(name_of_an_other_test) :- clause(largest(A,B,A),true).
:- end_tests(groupname).
```

Regular
[PLUnit](<https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)>)
can be used

## Form check

The form check checks the form of submissions. This is basically a linter.

### Test specification

It is not driven by a test file, but by the exercise configuration. Following
keys can be inclused in the `evaluation` directive:

- `cutallowed`: A boolean value indicationg if cuts are allowed (default false)
- `predefined`: List of predefined predicates in `Name/Arity` form, if this is
  set, `check:check` will be executed (may be the empty list)

The checkCheat test is always executed, it looks for ocurrences of
`dodonaevaluate` and marks them as erronous.

example config

```
...
 "evaluation":{
    "predefined" : ["word/8"],
    "cutallowed": false
 },
...
```

## Format of Simple Test testfiles

Compares the output (that is true/false) of the students predicates with that of
a supplied correct solution.

### Test specification

Test files ending in `.simple.pl` will automatically be be executed by this
file. Such a test file should be a module that exports one predicate: `test/6`.
The form is:

```prolog
%- consult public "database.pl".
:- module(dodonaevaluate, [tests/6]).

% ...

tests("markdown name",dodonaevaluate:thepredicate,thepredicate,true,10000, L) :-
    Pers = [waldo,odlaw,swi,wilma,whitebeard,woof,watchers],
    setof([X,Y],(member(X,Pers),member(Y,Pers)),L).
```

The `test/6` predicate has 6 arguments

1. The name, a string (double quotes), that is rendered as markdown
2. The functor name of a correct implementation (mostly of the form
   `dodonaevaluate:....`)
3. The functor name of the student implementation
4. A Boolean value indicating whether cuts are allowed (does not work properly yet)
5. An inference limit
6. A list of argument lists
   - Example for a one argument predicate : [[1],[2],[3]]
   - Example for a two argument predicate : [[1,a],[2,a],[3,a]]
   - Example for a one argument predicate that takes lists :
     [[[1,a]],[[2,a,9,8]],[[3,a,4]]

To make files available to both the user and test code, use one of the following
lines

```
%- consult public "file.pl".
%- consult private "file.pl".
%- consult public ["file1.pl","file2.pl"].
%- consult private ["file1.pl","file2.pl"].
```

### Implementation

The prolog checker code is contained in
[simpletest/checker.pl](simpletest/checker.pl). It is loaded together with the
student code as follows, with a copy of the workspace folder as workspace and
`dotests` as goals for `swipl`.

```
:- use_module("{judgePath}/simpletest/checker.pl").
:- consult("/mnt/.../submission/submission.pl").
:- use_module("/mnt/.../evaluation/.....simple.pl").
```

The results stored in `workspace/results.json` are then consulted to build the
feedback table

## Todos

- [ ] Using the partial format
- [ ] make text below clearer
