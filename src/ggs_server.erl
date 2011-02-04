%%%----------------------------------------------------
%%% @author     Jonatan Pålsson <Jonatan.p@gmail.com>
%%% @copyright  2010 Jonatan Pålsson
%%% @end
%%%----------------------------------------------------

-module(ggs_server).

% import
-import(ggs_network).

%% API
-export([start/0, start/1, stop/0 ]).
%-export([crash/0, vms/0, hello/0, echo/0]).
-export([get_count/0]).


%%%====================================================
%%% API
%%%====================================================

%%-----------------------------------------------------
%% @doc Starts the server
%% @end
%%-----------------------------------------------------
start() ->
    ggs_network:start_link().

start(Port) ->
    ggs_network:start_link(Port).


%%-----------------------------------------------------
%% @doc     Fetches the number of requests made to this server
%% @spec    get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%-----------------------------------------------------
get_count() ->
    ggs_network:get_count(get_count).

%crash()    -> gss_network:crash().
%vms() 	    -> gss_network:vms().
%hello()    -> gss_network:hello().
%echo()     -> gss_network:echo().

%%-----------------------------------------------------
%% @doc     Stops the server.
%% @spec    stop() -> ok
%% @end
%%-----------------------------------------------------
stop() ->
    ggs_network:stop().
