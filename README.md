# erlang-experiments

- - -

## Requirements

* Erlang (OTP) version: R15B

or

* Docker

## Usage

```
$ cd src
$ erl
1> c(hello).
2> hello:hallo("there").
3> halt().

```

### Using Docker

```
$ sudo systemctl start docker
$ sudo docker pull erlang
$ git clone https://github.com/hfagerlund/erlang-experiments.git
$ sudo docker run -it -v <absolute-path-to-git-clone-directory>/erlang-experiments/src:/app erlang
1> cd("app").
2> c(hello).
3> hello:hallo("Everyone").
4> halt().

# another example
1> cd("app").
2> c(add_up_shopping).
3> add_up_shopping:run().
```

## License
Copyright (c) 2018 Heini Fagerlund. Licensed under the [MIT License](https://github.com/hfagerlund/erlang-experiments/blob/master/LICENSE).

