FROM erlang:24-alpine

RUN mkdir /my-erlang-app
WORKDIR /my-erlang-app

COPY . .

RUN erlc ./src/hello.erl
RUN erl -noshell -run hello hallo there -s init stop
