-module(ggs_table).
-export([
    start_link/1,
    add_player/2,
    remove_player/2,
    stop/2,
    notify/3
]).

%% @doc This module represents a Player with a Socket and a Token

% @doc returns a new table
start_link(Token, Socket) ->
    spawn_link(fun(Token, Socket) ->
        GameVM = ggs_gamevm:start_link();
        loop(Token, Socket, GameVM, [])
    ).

% @doc adds a player to a table
add_player(Table, Player) ->
    ggs_logger:not_implemented().
    
% @doc removes player form a table
remove_player(Table, Player) ->
    ggs_logger:not_implemented().
    
% @doc stops the table process
stop(Table) ->
    Table ! {'EXIT', self(), normal}
    
% @doc notifies the table with a message from a player
notify(Table, Player, Message) ->
    Table ! {notify, Player, Message}.
    
       
% loop
loop(Token, Socket, GameVM, PlayerList) ->
    receive
        {add_player, Player} ->
            NewPlayerList = list:append(PlayerList, [Player]),
            loop(Token, Socket, GameVM, NewPlayerList);
        {remove_player, Player} ->
            NewPlayerList = list:delete(Player, PlayerList),
            loop(Token, Socket, GameVM, NewPlayerList);
        {notify, Player, Message} ->
            case Message of
                {server, define, Args} ->
                    ggs_gamevm:define(GameVM, Args),
                    loop(Token, Socket, GameVM, PlayerList);
                {game, Command, Args} ->
                    ggs_gamevm:user_command(GameVM, Player, Command, Args),
                    loop(Token, Socket, GameVM, PlayerList);
            end
    end.
    

% private helpers