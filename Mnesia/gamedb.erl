%Test Mnesia
-module(gamedb).
-import(mnesia).
-export([init/0,insert_player/1,example_player/0,read_player/1,test_player/0]).
-include("gamedb.hrl").

init() ->
    mnesia:create_table(player, [{attributes, record_info(fields, player)}]).

test_player() ->
    insert_player(example_player()),
    read_player(0001).

insert_player(Player) ->
    Fun = fun() ->
                  mnesia:write(Player) 
          end,
    mnesia:transaction(Fun).

example_player() ->
    #player{id   = 0001,
            name = "Tux"}.

read_player(Player_Key) ->
    Fun = fun() -> 
                  [P] = mnesia:read(player, Player_Key),
                  Name = P#player.name,
                  io:format("Player name: ~s~n",[Name])
          end,
    mnesia:transaction(Fun).

