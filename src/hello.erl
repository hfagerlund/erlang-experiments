% hello.erl

-module(hello).
-import(io,[format/1]).
-export([hallo/0]).

hallo() ->
	format("Hello~n"),
        format("again~n"),
	format("World~n").
