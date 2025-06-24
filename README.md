# erlang-experiments

Creating a portable Erlang app development environment on Linux [using Docker] and adding [development containers (dev containers)].

## Installation
Clone the repository using:
```
$ git clone https://github.com/hfagerlund/erlang-experiments.git
```
[Skip to 'Usage']

<!-- .................... -->
<details>
  <summary>Using Rebar3<strong>[+]</strong></summary>

* Install the latest stable compiled version of [Rebar3]:
```
$ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
```
* Generate the app skeleton by running:
```
$ sudo docker run --name $APP_NAME -it --rm -v ${PWD}:/app -w /app erlang ./rebar3 new app $APP_NAME
```
... where `$APP_NAME` = 'erlang_experiments'.
</details>
<!-- .................... -->

## Usage

### Docker Compose:

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
  # service-name:
  # ...
```

<a id="run-docker-compose"></a>Run docker-compose.yml as shown below:<br>
```
# start Docker
$ sudo systemctl start docker
# create and start the app
$ sudo docker compose up
```

#### Sample Output
With docker-compose.yml [running], do the following in another terminal tab:

```
$ sudo docker run -it --rm -v .:/app -w /app erlang ./rebar3 shell

===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlang_experiments
Erlang/OTP 28 [erts-16.0.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit:ns]

===> Booted erlang_experiments
Eshell V16.0.1 (press Ctrl+G to abort, type help(). for help)
1> c(hello).
Recompiling /app/src/hello.erl
{ok,hello}
2> eunit:test(erlang_experiments_test).
  All 3 tests passed.
ok
3> eunit:test(erlang_experiments_test, [verbose]).
======================== EUnit ========================
module 'erlang_experiments_test'
  erlang_experiments_test: hello_test...ok
  erlang_experiments_test: hello_again_test...ok
  erlang_experiments_test:14: -another_hello_test_/0-fun-0- (This test should also pass)...ok
  [done in 0.009 s]
=======================================================
  All 3 tests passed.
ok
4> halt().

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
[match this line]: https://github.com/hfagerlund/erlang-experiments/blob/71c50843eca7c9f9e8258a8f1c3c4330e819f3dd/docker-compose.yml#L12
[MIT License]: https://github.com/hfagerlund/erlang-experiments/blob/master/LICENSE
[running]: #run-docker-compose
[Skip to 'Usage']: #usage

<!-- external -->
[development containers (dev containers)]: https://containers.dev/
[extensions marketplace]: https://marketplace.visualstudio.com/search?target=VSCode&category=Extension%20Packs&sortBy=Installs
[open specification]: https://containers.dev/
[Rebar3]: https://github.com/erlang/rebar3
[using Docker]: https://www.docker.com/

