-module(ggs_coordinator).

%% API Exports
-export([start_link/0, stop/1, join_table/1, create_table/1]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).
-define(SERVER, ?MODULE).

%% @doc This module act as "the man in the middle". 
%%	Creates the starting connection between table and players.

%% @doc Starts the coordinator process.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Terminates the coordinator process.
stop(Reason) ->
    gen_server:cast(ggs_coordinator, {stop, Reason}).

%% @doc Joins table with specified token, returns {error, no_such_table}
%% if the specified table token does not exist
join_table(Token) ->
    gen_server:call(ggs_coordinator, {join_table, Token}). 

%% @doc Create a new table, return {error, Reason} or {ok, TableToken} 
create_table(Params) ->
    gen_server:call(ggs_coordinator, {create_table, Params}). 

%% @doc This is the first function run by a newly created players. 
%%	Generates a unique token that we use to identify the player.
join_lobby() -> 
    ggs_logger:not_implemented().

%% @doc Act as a supervisor to player and respawns player when it gets bad data.
respawn_player(_Player, _Socket) ->
    ggs_logger:not_implemented().

%% @doc Act as a supervisor to table and respawns table when it gets bad data.
respawn_table(_Token) ->
    ggs_logger:not_implemented().

%% @doc Removes a player from coordinator.
remove_player(_From, _Player) -> 
    ggs_logger:not_implemented().

%% gen_server callbacks

init([]) ->
    {ok, ok}.

handle_call({join_table, Table}, From, State) ->
    {reply, {error, no_such_table}, State};

handle_call({create_table, {force, TID}}, From, State) ->
    {reply, {ok, TID}, State};

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast({stop, Reason}, State) ->
    {stop, normal, state};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
