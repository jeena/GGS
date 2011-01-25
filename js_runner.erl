-module(js_runner).
-export([executeJS/2, boot/0]).

boot() ->
    erlang_js:start(),
    {ok, Port} = js_driver:new(),
    Port.

executeJS(Port, Data) ->
    ok = js:define(Port, <<"function helloworld(name){return 'Hello, ' + name}">>),
    js:call(Port, <<"helloworld">>, [Data]).
