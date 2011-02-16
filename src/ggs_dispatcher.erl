-module(ggs_dispatcher).
-export([start_link/1, stop/1]).

%% @doc This module is the entry-point for clients connecting to GGS. This is
%% the module responsible for:
%%  * Greeting a connecting client, and associating a socket for it
%%  * Spawning a ggs_player for the connecting client, passing the socket

%% @doc Starts a new dispatcher with the specified port. Registers this 
%% dispatcher under the name "ggs_dispatcher". The pid of the dispatcher
%% is returned.
%% @spec start_link(Port) -> Pid
%%      Port = Integer
%%      Pid = #<Pid>
start_link(Port) ->
    not_implemented.

%% @doc Stops the dispatcher with the specified reason.
%% @spec stop(Reason) -> ok.
%%      Reason = String
stop(Reason) -> not_implemented.
