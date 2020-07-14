-module(add_up_shopping_tests).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
       ?_test(?assert(1 + 1 =:= 2)).

subtotal_test() ->
    ?assertEqual([{item1, 1.00}, {item2, 10.00}], add_up_shopping:subtotal([{item1, 1, 1.00}, {item2, 2, 5.00}])).
