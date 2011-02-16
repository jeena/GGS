-module(ggs_coordinator).
-export().

%% @doc This module act as "the man in the middle". 
%%	Creates the starting connection between table and players.

%% @doc Starts the coordinator process.
start_link() ->
    not_implemented.

%% @doc Terminates the coordinator process.
stop() ->
    not_implemented.

%% @doc Creates a unique token for the table. 
join_table() ->
    not_implemented.

%% @doc 
create_table() ->
    not_implemented.

%% @doc This is the first function run by a newly created players. 
%%	Generates a unique token that we use to identify the player.
join_lobby() -> 
    not_implemented.

%% @doc Act as a supervisor to player and respawns player when it gets bad data.
respawn_player(Player, Socket) ->
    not_implemented.

%% @doc Act as a supervisor to table and respawns table when it gets bad data.
respawn_table() ->
    not_implemented.

%% @doc Removes a player from coordinator.
remove_player(From, Player) -> 
    not_implemented.
