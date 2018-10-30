-module(hello).
-export([hello/0]).

hello() ->
	io:format("Hello~n"),
	io:format("World~n").
