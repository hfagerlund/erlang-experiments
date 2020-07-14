-module(hello_tests).
-include_lib("eunit/include/eunit.hrl").

passing_test() -> ?assert(true).
%failing_test() -> ?assert(false).

call_hallo_test() -> hello:hallo(["world"]).
