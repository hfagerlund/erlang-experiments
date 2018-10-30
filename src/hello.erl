% hello.erl

-module(hello).
-import(io,[format/1]).
-export([hello/0]).

hello() ->
	format("Hello~n"),
        format("again~n"),
	format("World~n").
