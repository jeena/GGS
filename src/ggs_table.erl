%% @doc This module represents a Player with a Socket and a Token

-module(ggs_table).
-export([
    start_link/1,
    add_player/2,
    remove_player/2,
    stop/2,
    notify/3
]).

% @doc returns a new table
start_link(Token) ->
    not_implemented().

% @doc adds a player to a table
add_player(Table, Player) ->
    not_implemented().
    
% @doc removes player form a table
remove_player(Table, Player) ->
    not_implemented().
    
% @doc stops the table process
stop(Table) ->
    not_implemented().
    
% @doc notifies the table with a message from a player
notify(Table, Player, Message) ->
    not_implemented().
    
       
% loop


% private helpers