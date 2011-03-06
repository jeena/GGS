-module(ggs_coordinator).

%% API Exports
-export([   start_link/0, 
            stop/1, 
            join_table/1, 
            create_table/1, 
            join_lobby/0,
            respawn_player/2, 
            respawn_table/1, 
            remove_player/2, 
            get_all_players/0,
            table_token_to_pid/1,
            table_pid_to_token/1,
            player_pid_to_token/1,
            player_token_to_pid/1]).

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

get_all_players() ->
    gen_server:call(?SERVER, get_all_players).


%% Conversion tools

table_token_to_pid(Token) ->
    gen_server:call(?SERVER, {table_token_to_pid, Token}).

table_pid_to_token(Pid) ->
    gen_server:call(?SERVER, {table_pid_to_token, Pid}).

player_pid_to_token(Pid) ->
    gen_server:call(?SERVER, {player_pid_to_token, Pid}).

player_token_to_pid(Token) ->
    gen_server:call(?SERVER, {player_token_to_pid, Token}).

%% Just to shorten the name
back_up(State) ->
    ggs_coordinator_backup:back_up(State),
    State.

%% gen_server callbacks

init([]) ->
    % Restore old state from backup if there is old state stored there
    case ggs_coordinator_backup:retrieve() of
        no_state_stored ->
            io:format("No old state stored.. Creating new!~n"),
            {ok, #co_state{}};
        State ->
            {ok, State}
    end.

handle_call(join_lobby, From, State) ->
    Token = helpers:get_new_token(),
    Players = State#co_state.players,
    {Pid, _Sock} = From,
    NewState = State#co_state{players = [{Pid, Token} | Players]},
    back_up(NewState),
    {reply, {ok, Token}, NewState};

handle_call({join_table, Table}, From, State) ->
    {FromPlayer, _Ref}  = From,
    Tables = State#co_state.tables,
    case lists:keyfind(Table, 1, Tables) of
        {_TableID, TablePID} ->
            ggs_table:add_player(TablePID, FromPlayer),
            back_up(State),
            {reply, {ok, TablePID}, State}; 
        false ->
            back_up(State),
            {reply, {error, no_such_table}, State}
    end;

handle_call({create_table, {force, TableToken}}, From, State) ->
    TableIDMap          = State#co_state.player_table_map,
    Tables              = State#co_state.tables,
    NewTableProc        = ggs_table:start(TableToken), % With start_link, the table dies with the coordinator
    NewState = State#co_state{
                            player_table_map = [{From, TableToken} | TableIDMap],
                            tables           = [{TableToken, NewTableProc} | Tables]
                            },
    back_up(NewState),
    {reply, {ok, TableToken}, NewState};

handle_call(get_all_players, _From, State) ->
    {reply, State#co_state.players, State};

%% Conversion tools
handle_call({table_token_to_pid, Token}, _From, State) ->
    Tables = State#co_state.tables,
    {_, Pid} = lists:keyfind(Token, 1, Tables),
    {reply, Pid, State};

handle_call({table_pid_to_token, Pid}, _From, State) ->
    Tables = State#co_state.tables,
    {Token, _} = lists:keyfind(Pid, 2, Tables),
    {reply, Token, State};

handle_call({player_pid_to_token, Pid}, _From, State) ->
    Players = State#co_state.players,
    {Pid, Token} = lists:keyfind(Pid, 1, Players),
    {reply, Token, State};

handle_call({player_token_to_pid, Token}, _From, State) ->
    Players = State#co_state.players,
    {Pid, Token} = lists:keyfind(Token, 2, Players),
    {reply, Pid, State};

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
