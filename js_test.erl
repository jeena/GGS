-module(js_test).
-export([run/0]).
run() ->
    erlang_js:start(),
    {ok, Port} = js_driver:new(),
    ok = js:define(Port, <<"function helloworld(name){return 'Hello, ' + name}">>),
    js:call(Port, <<"helloworld">>, [<<"Generic Game Server">>]).
