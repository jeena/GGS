-module(ggs_coordinator).

%% API Exports
-export([start_link/0, stop/1, join_table/1, create_table/1, join_lobby/0,
         respawn_player/2, respawn_table/1, remove_player/2]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).
-define(SERVER, ?MODULE).

-record(co_state,
            {players            = [],   % List of all player processes
             player_table_map   = [],   % Players <-> Table map
             table_state_map    = [],
             tables             = []}). % Table <-> Table state map

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
    gen_server:call(ggs_coordinator, join_lobby). 

%% @doc Act as a supervisor to player and respawns player when it gets bad data.
respawn_player(_Player, _Socket) ->
    ggs_logger:not_implemented().

%% @doc Act as a supervisor to table and respawns table when it gets bad data.
respawn_table(_Token) ->
    ggs_logger:not_implemented().

%% @doc Removes a player from coordinator.
remove_player(_From, _Player) -> 
    %gen_server:cast(ggs_coordinator, {remove_player, Player}).
    ggs_logger:not_implemented().

%% gen_server callbacks

init([]) ->
    {ok, #co_state{}}.

handle_call(join_lobby, _From, State) ->
    Token = helpers:get_new_token(),
    {reply, {ok, Token}, State};

handle_call({join_table, Table}, From, State) ->
    {FromPlayer, _Ref}  = From,
    Tables = State#co_state.tables,
    case lists:keyfind(Table, 1, Tables) of
        {_TableID, TablePID} ->
            ggs_table:add_player(TablePID, FromPlayer),
            {reply, {ok, TablePID}, State}; 
        false ->
            {reply, {error, no_such_table}, State}
    end;

handle_call({create_table, {force, TableID}}, From, State) ->
    TableIDMap          = State#co_state.player_table_map,
    Tables              = State#co_state.tables,
    NewTableProc        = ggs_table:start_link(),
    {reply, {ok, TableID}, State#co_state{
                                        player_table_map = [{From, TableID} | TableIDMap],
                                        tables           = [{TableID, NewTableProc} | Tables]
                                        }};

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast({stop, _Reason}, State) ->
    {stop, normal, State};

%% @TODO: Implement me
%handle_cast({remove_player, Player}) ->
%    {noreply, State#co_state{
                                

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
