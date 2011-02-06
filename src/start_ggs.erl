-module(start_ggs).
-export([start/0]).

start() ->
    application:start(inets),
    application:start(erlang_js),
    application:start(ggs).
