-module(ggs_table_test).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
	Table = ggs_table:start_link(),
	?assertNot(Table =:= undefined).

add_player_test() ->
	Table = ggs_table:start_link(),
	Player = test_player,
	ggs_table:add_player(Table, Player),
	{ok, [Player]} = gen_server:call(Table, get_player_list).

remove_player_test() ->
	Table = ggs_table:start_link(),
	Player = test_player,
	Player2 = test_player2,
	ggs_table:add_player(Table, Player),
	{ok, [Player]} = gen_server:call(Table, get_player_list),
	ggs_table:add_player(Table, Player2),
	{ok, [Player2, Player]} = gen_server:call(Table, get_player_list),	
	ggs_table:remove_player(Table, Player),
	{ok, [Player2]} = gen_server:call(Table, get_player_list),
	ggs_table:remove_player(Table, Player2),
	{ok, []} = gen_server:call(Table, get_player_list).	

stop_test() ->
	Table = ggs_table:start_link(),
	ok = ggs_table:stop(Table).

notify_test() ->
	Table = ggs_table:start_link(),
	Player = test_player,
	Message = {server, define, "function helloWorld(x) {  }"},
	ok = ggs_table:notify(Table, Player, Message),
	Message2 = {game, "helloWorld", "test"},
	ok = ggs_table:notify(Table, Player, Message2).
   
