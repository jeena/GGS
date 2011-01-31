-module(v8test).
-export([run/0]).
-include_lib("erlv8/include/erlv8.hrl").

run() ->
    application:start(erlv8),
    {ok, VM} = erlv8_vm:start(),
    Global = erlv8_vm:global(VM),
    Global:set_value("callback",  erlv8_object:new([{"exec", fun (#erlv8_fun_invocation{}, []) -> myCallback() end}])),
    erlv8_vm:run(VM,"callback.exec();").

myCallback() ->
    io:format("Hello from myCallback!~n", []),
    ok.
