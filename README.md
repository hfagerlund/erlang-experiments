# erlang-experiments

- - -
<table>
<tr>
<td> hello.erl </td> <td> add_up_shopping.erl </td>
</tr>

<tr>
<td>
output:<br>
Displays the customized greeting 'Hello {name}', where
{name} is input by the user.
</td>
<td>
output:<br>
Calculates subtotal for each item in shopping list (eg. milk)
by multiplying the number of items with its per unit price.
</td>
</tr>

<tr>
<td>

```

1> cd("app").
/app
ok
2> c(hello).
{ok,hello}
3> hello:hallo("Everyone").
Hello Everyone
ok
4> halt().

```
</td>
<td>

```

1> cd("app").
/app
ok
2> c(add_up_shopping).
{ok,add_up_shopping}
3> add_up_shopping:run().
[{milk,5.99},{butter,13.98},{flour,11.97}]
ok
4> halt().

```
</td>
</tr>
</table>

*Fig. 1 - Eshell outputs of demo Erlang programs (included in [/src] directory)*
- - -

## Requirements

* Erlang version: R15B

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

# run (EUnit) tests
$ sudo docker run -it -v <absolute-path-to-git-clone-directory>/erlang-experiments/:/app erlang
1> cd("app").
2> make:all([load]).
3> eunit:test(hello).
4> eunit:test(add_up_shopping).
5> halt().
```

## License
Copyright (c) 2018 Heini Fagerlund. Licensed under the [MIT License](https://github.com/hfagerlund/erlang-experiments/blob/master/LICENSE).

