# judge-prolog

Currently a PLUnit tester only

Can be tested with:

```bash
docker run --rm  -v $(realpath .):/home/runner/workdir dodona-prolog ./test/test.bash
```

After a build using

```bash
docker build -t dodona-prolog .
```


The `main.sh` file is form the [docker-images](https://github.ugent.be/dodona/docker-images) repo, it should not be edited and will be removed.


## Todos

- [x] Memory management
- [ ] Using the partial format
- [x] Add quickcheck (see the `feature/quickcheck` branch and PR)
- [ ] make text below clearer

## Format of the Quickcheck tests

All tests must have a name starting with `prop_`. There should be at least one blank line sparating the definition from other thing. Tests may contain comments. Comments within the test will be shown to the student, comments outside it wil not.

## Format of PLUnit testfiles

The file will be splitted up in parts between the tags `:- begin_tests(...).` and `:- end_tests(...).`. All the lines specified between the parst above the current part wil be used.



# Availible tests

## Form check

The form check checks the form of submissions. This is absicly a linter

### Test specification

It is not driven by a test file,
but by the exercise configuration. Following keys can be inclused in the
`evaluation` directive:

- `cutallowed`: A boolean value indicationg if cuts are allowed (default false)
- `predefined`: List of predefined predicates in `Name/Arrity` form, if this is
   set, `check:check` will be executed (may be the empty list)

The checkCheat test is always executed, it looks for obeccurences of
`dodonaevaluate` and marks them as errorous.

example config
```
...
 "evaluation":{
    "predefined" : ["word/8"],
    "cutallowed": false
 },
...
```

## Simple test

Compares the output (that is true/false) of the students predicates with that
of a verified correct solution.

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
2. The functor name of a correct implementation (mostly of the form `dodonaevaluate:....`)
3. The functor name of the student implementation
4. A Boolean value indicating is cuts are allowed (does not work properly yet)
5. An inference limit
6. A list of argument lists
    - Example for a one argument predicate : [[1],[2],[3]]
    - Example for a two argument predicate : [[1,a],[2,a],[3,a]]
    - Example for a one argument predicate that takes lists : [[[1,a]],[[2,a,9,8]],[[3,a,4]]

To make files available to both the user and test code, use one of the following lines

```
%- consult public "file.pl".
%- consult private "file.pl".
%- consult public ["file1.pl","file2.pl"].
%- consult private ["file1.pl","file2.pl"].
```
### Implementation

The prolog checker code is contained in [simpletest/checker.pl](simpletest/checker.pl) it is loaded together
with the student code as follows, with a copy of the workspace folder as workspace and `dotests` as 
goals for `swipl`.

```
:- use_module("{judgePath}/simpletest/checker.pl").
:- consult("/mnt/.../submission/submission.pl").
:- use_module("/mnt/.../evaluation/.....simple.pl").
```

The results stored in `workspace/results.json` are then consulted to build the feedback table