% hello.erl

-module(hello).
-import(string,[concat/2]).
-export([hallo/1]).

hallo(N) ->
	X = "Hello ",
        Y = concat(X, N),
        io:fwrite([Y]),
        io:fwrite("~n").
