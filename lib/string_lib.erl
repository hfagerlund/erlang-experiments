-module(string_lib).
-export([format/2]).

%% custom definition
format(Template, Params) ->
    lists:flatten(io_lib:fwrite(Template, Params)).
