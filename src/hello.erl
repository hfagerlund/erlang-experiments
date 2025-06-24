% hello.erl

-module(hello).
-export([hallo/1]).

- spec(hallo(string()) -> string()).

%% custom definition
format(Template, Params) ->
    lists:flatten(io_lib:fwrite(Template, Params)).

hallo(N) ->
    format("Hello ~s!", [N]).
