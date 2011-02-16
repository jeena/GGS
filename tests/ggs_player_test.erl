-include_lib("eunit/include/eunit.hrl").
-import(ggs_player).

%% @doc start_link should always return ok for any valid socket. A valid socket 
%% should always return {ok, Pid} and {error, Reason} otherwise.
start_link_test() ->
    helpers:not_implemented().

%% @doc Given that start_link returned {ok, Player}. Notify shall always return ok and
%% deliver a specified message through the socket.
notify_test() ->
    helpers:not_implemented().
                                                  
%% @doc Given that start_link returned {ok, Player}. get_token shall always return a valid
%% player token. a valid token should be unique.
get_token_test() ->
    helpers:not_implemented().

%% @doc Given that start_link returned {ok, Pid}. There shouldn't be possible to 
%% execute this function with the same Player and Table arguments twice.
stop_test() ->
    helpers:not_implemented().
    
