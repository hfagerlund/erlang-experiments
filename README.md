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

### Local Installation
Using the **Erlang shell** (`erl`):

```
$ cd src
$ erl
1> c(hello).
2> hello:hallo("there").
3> halt().

```

### Docker
* Using Docker container's **interactive shell** (`-it`):

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
4> halt().

# run (EUnit) tests
$ sudo docker run -it -v <absolute-path-to-git-clone-directory>/erlang-experiments/:/app erlang
1> cd("app").
2> make:all([load]).
3> eunit:test(hello).
4> eunit:test(add_up_shopping).
5> halt().
```
- - -

#### Dockerfile:

* Using `Dockerfile` (below) -
```
# Updated official Erlang image from Docker Hub
FROM erlang:24-alpine

# Set working directory
RUN mkdir /my-erlang-app
WORKDIR /my-erlang-app

# OPTIONAL: Uncomment the line below to keep container running indefinitely (for testing, debugging)
## CMD ["sleep", "infinity"]

# Copy application source code
COPY . .

# compile small Erlang program 'hello.erl' (to produce a BEAM file that can execute on the Erlang VM):
RUN erlc ./src/hello.erl

# run sequential Erlang program as a common script, and exit
RUN erl -noshell -run hello hallo there -s init stop
## ...where:
##    'hello' = module,
##    'hallo' = function,
##    'there' = argument (passed to the program)
```

Run the following in the **same directory** as `Dockerfile`:
```
# build Docker image
$ sudo docker build -t my-erlang-app .

# launch a new Docker container based on the image
$ sudo docker run --name my-test-erlang-container my-erlang-app
```

Docker creates a container and executes the [command] listed in the image's Dockerfile (equivalent to `hello:hallo("there")`).

output:<br>
```
$ sudo docker build -t my-erlang-app .
# ...
[+] Building 16.4s (11/11) FINISHED
 => [internal] load build definition from Dockerfile                                                                                                     0.5s
 => => transferring dockerfile: 199B                                                                                                                     0.0s
 => [internal] load .dockerignore                                                                                                                        0.4s
 => => transferring context: 2B                                                                                                                          0.0s
 => [internal] load metadata for docker.io/library/erlang:24-alpine                                                                                      2.4s
 => [1/6] FROM docker.io/library/erlang:24-alpine@sha256:xxx                                1.6s
 => => resolve docker.io/library/erlang:24-alpine@sha256:xxx                                1.6s
 => [internal] load build context                                                                                                                        0.2s
 => => transferring context: 56.72kB                                                                                                                     0.1s
 => [2/6] RUN mkdir /my-erlang-app                                                                                                                0.0s
 => [3/6] WORKDIR /my-erlang-app                                                                                                                  0.0s
 => [4/6] COPY . .                                                                                                                                       1.0s
 => [5/6] RUN erlc ./src/hello.erl                                                                                                                       3.0s
 => [6/6] RUN erl -noshell -run hello hallo there -s init stop                                                                                           4.1s
# '=> => # Hello there' briefly flashes in terminal
 => exporting to image                                                                                                                                   1.9s
 => => exporting layers                                                                                                                                  1.8s
 => => writing image sha256:xxx                                                             0.0s
 => => naming to docker.io/library/my-erlang-app

$ sudo docker run --name my-test-erlang-container my-erlang-app
Eshell V12.3.2.17  (abort with ^G)
1> *** Terminating erlang (nonode@nohost)
```

## License
Copyright (c) 2018 Heini Fagerlund. Licensed under the [MIT License](https://github.com/hfagerlund/erlang-experiments/blob/master/LICENSE).

<!-- References -->
[command]: https://github.com/hfagerlund/erlang-experiments/blob/29a853b0e62e1115830e68edb172075022a172cf/Dockerfile#L9
