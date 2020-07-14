% add_up_shopping.erl

-module(add_up_shopping).
-export([subtotal/1]).

subtotal(List) ->
	[{Item, Price * Quantity} || {Item, Quantity, Price} <- List].
