%%%-------------------------------------------------------------------
%% @doc erlang_experiments public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_experiments_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_experiments_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
