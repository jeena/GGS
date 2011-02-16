-module(ggs_coordinator).

%% API Exports
-export([start_link/1, stop/1]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).

%% @doc This module act as "the man in the middle". 
%%	Creates the starting connection between table and players.

%% @doc Starts the coordinator process.
start_link() ->
    not_implemented.

%% @doc Terminates the coordinator process.
stop(_Reason) ->
    not_implemented.

%% @doc Joins table with specified token
join_table(_Token) ->
    not_implemented.

%% @doc Create a new table 
create_table(_Params) ->
    not_implemented.

%% @doc This is the first function run by a newly created players. 
%%	Generates a unique token that we use to identify the player.
join_lobby() -> 
    not_implemented.

%% @doc Act as a supervisor to player and respawns player when it gets bad data.
respawn_player(_Player, _Socket) ->
    not_implemented.

%% @doc Act as a supervisor to table and respawns table when it gets bad data.
respawn_table(_Token) ->
    not_implemented.

%% @doc Removes a player from coordinator.
remove_player(From, Player) -> 
    not_implemented.

%% gen_server callbacks

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
