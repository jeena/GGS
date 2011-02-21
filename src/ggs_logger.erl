-module(ggs_logger).
-export([not_implemented/0, log/2]).

not_implemented() ->
    exit(not_implemented).

log(Format, Args) ->
    error_logger:info_msg(Format, Args).
