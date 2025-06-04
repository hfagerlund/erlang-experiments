% add_up_shopping.erl

-module(add_up_shopping).
-export([subtotal/1]).

subtotal(ShoppingList) ->
	ShoppingList = [{milk, 1, 5.99}, {butter, 2, 6.99}, {flour, 3, 3.99}],
        ItemSubtotal = [{Item, Price * Quantity} || {Item, Quantity, Price} <- ShoppingList],
        io:format("~p~n", [ItemSubtotal]).
