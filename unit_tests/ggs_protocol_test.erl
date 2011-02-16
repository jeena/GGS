-module(ggs_protocol_test).
-export([test_parse/0]).

test_parse() ->
    Ret = ggs_protocol:parse("<> __define JavaScript"),
    io:format("~p~n", [Ret]).
