-module(ggs_player).
-export([start_link/1, notify/3, get_token/1, stop/1]).

%% @doc This module handles communication between a player and GGS. This module is
%%responsible for: 
%% * The storage of the player socket, player token and a table token.
%% * Ability to fetch a player token.
%% * Forwarding messages from players to the game
%% * Remove a player from GGS

%% @doc Spawns a process representing the player in GGS. Takes the player socket as
%% an argument for storage and later usage. Creates a unique player token
%% identifying the player.
%% @spec start_link(Socket::socket()) -> ok
start_link(Socket) -> not_implemented.


%% @doc Handles incoming messages from the GGS and forwards them through the player
%% socket to the player.
%% @spec notify(Player::Pid(), From::Pid(), 
%%              {Command::String(), Message::string()}) -> ok
notify(Player, From, Message) -> not_implemented.

%% @doc Get the player token uniquely representing the player.
%% @spec get_token() -> string()
get_token() -> not_implemented.


%% @doc Properly terminates the player process. The player token will be destroyed. 
%% Makes table token unreferenced and destroys the process in the end.
%% @spec stop(Table::pid()) -> Reason::string()
stop(Table) -> not_implemented.
