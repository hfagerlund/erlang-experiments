% hello.erl

-module(hello).
-import(string_lib,[format/2]).
-export([hallo/1]).

- spec(hallo(string()) -> string()).

hallo(N) ->
    format("Hello ~s!", [N]).
