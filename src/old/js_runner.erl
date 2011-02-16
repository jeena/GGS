-module(js_runner).
-export([define/2,call/3, boot/0]).

boot() ->
    erlang_js:start(),
    {ok, Port} = js_driver:new(),
    Port.

define(Port, Data) ->
    ok = js:define(Port, list_to_binary(Data)).
    
call(Port, Func, Params) ->
    js:call(Port, list_to_binary(Func), Params).
