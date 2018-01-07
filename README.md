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

- [ ] Memory management
- [ ] Using the partial format
- [ ] Add quickcheck (see the `feature/quickcheck` branch and PR)
