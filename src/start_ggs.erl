-module(start_ggs).
-export([start/0]).

start() ->
    application:start(inets),
    application:start(erlang_js),
    %ggs_stats:start_link(),
    ggs_db:init(),
    application:start(ggs).
