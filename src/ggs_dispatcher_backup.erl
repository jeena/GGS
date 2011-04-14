-module(ggs_dispatcher_backup).

-behaviour(gen_server).

%% API Exports
-export([start_link/0, stop/1]).
-export([back_up/1, retrieve/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).

-define(SERVER, ?MODULE).
%% @doc This module is responsible for keeping a backup of the coodinator
%% at all times. At any point in time a backup can be restored from this
%% module.
%% This module is started by the root supervisor, and is restarted when it
%% crashes. Upon a crash, the backup state is lost in this module, and must
%% be filled in from the ggs_coordinator.

%% @doc Start a new ggs_coordinator backup instance, and register it under
%% this name. This means that there can only be one instance of this module
%% running.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the server with the specified reason.
%% @spec stop(Reason) -> ok.
%%      Reason = String
stop(_Reason) -> ggs_logger:not_implemented().

%% API
back_up(State) ->
    gen_server:cast(?SERVER, State).

%% @doc Retrieve the state stored in this server. If there is a state stored
%% here, it is returned to the caller. If the backup server does not have a
%% state stored, it will return the no_state_stored atom.
retrieve() ->
    gen_server:call(?SERVER, retrieve).

%% gen_server callbacks

%% @doc Initiate the server. This is called from gen_server
init([]) ->
    {ok, no_state_stored}.

handle_call(retrieve, _From, State) ->
    {reply, State, State}.

handle_cast(NewState, _State) ->
    {noreply, NewState}.

handle_info(Msg, State) ->
    io:format("Received out of bounds message! "),
    erlang:display(Msg),
    io:format("~n"),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
