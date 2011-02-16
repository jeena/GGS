-module(ggs_player).
-export([start_link/1, notify/3, get_token/1, stop/1]).

%% @doc This module handles communication between a player and GGS. This module is
%%responsible for: 
%% * the storage of the player socket, player token and a table token.

%% @doc Spawns a new player process.
%% @spec start_link(Socket::socket()) -> ok
start_link(Socket) -> not_implemented.


%% @spec notify(Player::Pid(), From::Pid(), 
%%              {Command::String(), Message::string()}) -> ok
%% @doc send a message to a player.
notify(Player, From, Message) -> not_implemented.


%% @spec get_token() -> string()
%% @doc Get the player token.
get_token() -> not_implemented.


%% @spec stop(Table::pid()) -> Reason::string()
%% @doc Properly terminates the process.
stop(Table) -> not_implemented.
