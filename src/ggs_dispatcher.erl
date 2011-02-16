-module(ggs_dispatcher).

%% API Exports
-export([start_link/1, stop/1]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).


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


%% gen_server callbacks

%% @doc Initiate the dispatcher. This is called from gen_server
init([Port]) ->
    {ok, ok}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, Extra) ->
    {ok, State}.
