-module(js_helper).
-export([test/0, echo/1]).

test() ->
	{ok, test}.
	
echo(String) ->
	{ok, String}.