# erlang-experiments

Creating a portable Erlang app development environment on Linux [using Docker] and adding [development containers (dev containers)].

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
## Table of contents
* [Local Installation]
* [Docker]
  * [Docker CLI]
  * [Dockerfile]
  * [docker-compose]
* [Dev Container]
  * [VS Code]
- - -
## Usage

### Local Installation
*(Erlang version: R15B)*

Using the **Erlang shell** (`erl`):

```
$ cd src
$ erl
1> c(hello).
2> hello:hallo("there").
3> halt().

```

### Docker
*(Docker version 24.0.2)*
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
- - -
#### docker-compose:

* Using `docker-compose.yml` (below) -
```
version: "3.3"
services:
  ####################################################
  # Web app
  ####################################################
  web:
    build:
      context: .
      dockerfile: Dockerfile
    ports:
      - "80:80"
    volumes:
      # where VSCode looks for project's source code
      # same as 'workspaceFolder' in .devcontainer/devcontainer.json
      - ..:/workspace
    # keep container running (for testing)
    command: "sleep infinity"
  ####################################################
  # another service
  ####################################################
  # rebar:
  # ...
```

Run docker-compose.yml as shown below:<br>
```
# start Docker
$ sudo systemctl start docker
# create and start the app
$ sudo docker compose up
```
- - -
### Dev Container

Dev Containers are an [open specification], and can be used without Visual Studio Code (VS Code), and/or without Docker.

A '**/.devcontainer**' directory, and '**/.devcontainer/devcontainer.json**' file are required.

For example:<br>
`devcontainer.json` (below) -
```
{
    "name": "Erlang dev container",
    "dockerComposeFile": "../docker-compose.yml",
    "workspaceFolder": "/workspace",
    "service": "web"
}
```
> [!NOTE]
> Value of *'workspaceFolder'* should [match this line] in `docker-compose.yml`.

#### Using Dev Container with VS Code
1. **Install VS Code extension**:<br>
Install '**Visual Studio Code Remote Development Extension Pack**' in Visual Studio Code from [extensions marketplace].

2. **User Settings**:<br>
Open Visual Studio Code (with extension installed):
  * Ctrl + Shift + P > *type*: open settings > select '`Preferences: Open User Settings (JSON)`' > opens 'settings.json' file > save file in default location

  For example:<br>
`settings.json` (below) -
```
$ cat ~/.config/Code/User/settings.json
{
    "otherhost": "/foobar"
}
```

3. **Start Docker**:<br>
Make sure Docker is running, and correct **file permissions** are granted:
```
$ sudo systemctl start docker
$ sudo chown -R $(whoami) ~/.docker
```

4. **Run dev container**:<br>
Open Visual Studio Code (with extension installed):
  * Open **root directory** of project repo (`$ git clone https://github.com/hfagerlund/erlang-experiments.git`))
  * Ctrl + Shift + P > select '`Dev Containers: run in container`' command

> [!TIP]
> Use VS Code's `Dev Containers: Rebuild Container` command for your container to update if contents of the `.devcontainer` directory have been modified.

- - -
## License
Copyright (c) 2018 Heini Fagerlund. Licensed under the [MIT License].

<!-- References -->
<!-- internal -->
[Local Installation]: #local-installation
[Docker]: #docker
[Docker CLI]: #docker
[Dockerfile]: #dockerfile
[docker-compose]: #docker-compose
[Dev Container]: #dev-container
[VS Code]: #using-dev-container-with-vs-code

[command]: https://github.com/hfagerlund/erlang-experiments/blob/29a853b0e62e1115830e68edb172075022a172cf/Dockerfile#L9
[match this line]: https://github.com/hfagerlund/erlang-experiments/blob/71c50843eca7c9f9e8258a8f1c3c4330e819f3dd/docker-compose.yml#L12
[MIT License]: https://github.com/hfagerlund/erlang-experiments/blob/master/LICENSE
[/src]: https://github.com/hfagerlund/erlang-experiments/tree/master/src

<!-- external -->
[development containers (dev containers)]: https://containers.dev/
[extensions marketplace]: https://marketplace.visualstudio.com/search?target=VSCode&category=Extension%20Packs&sortBy=Installs
[open specification]: https://containers.dev/
[using Docker]: https://www.docker.com/

