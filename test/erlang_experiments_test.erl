-module(erlang_experiments_test).
-import(hello, [hallo/1]).
-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual("Hello world!", hello:hallo("world")).

hello_again_test() ->
    ?assertNotEqual("Hello world", hello:hallo("world")).

another_hello_test_() ->
    {"This test should also pass",
     ?_test(hello_again_test())}.
