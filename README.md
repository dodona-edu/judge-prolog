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
