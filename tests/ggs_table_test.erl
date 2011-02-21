-include_lib("eunit/include/eunit.hrl").
-import(ggs_table).


% @private
start_link_test() ->
	Table = start_link(),
	?assertNot(Table =:= undefined).

% @private   
add_player_test() ->
	Table = start_link(),
	Player = test_player,
	add_player(Table, Player),
	{ok, [Player]} = gen_server:call(Table, get_player_list).

% @private  
remove_player_test() ->
	Table = start_link(),
	Player = test_player,
	Player2 = test_player2,
	add_player(Table, Player),
	{ok, [Player]} = gen_server:call(Table, get_player_list),
	add_player(Table, Player2),
	{ok, [Player2, Player]} = gen_server:call(Table, get_player_list),	
	remove_player(Table, Player),
	{ok, [Player2]} = gen_server:call(Table, get_player_list),
	remove_player(Table, Player2),
	{ok, []} = gen_server:call(Table, get_player_list).	

% @private	
stop_test() ->
	Table = start_link(),
	ok = stop(Table).

% @private
notify_test() ->
	Table = start_link(),
	Player = test_player,
	Message = {server, define, "function helloWorld(x) {  }"},
	ok = notify(Table, Player, Message).
	Message2 = {game, "helloWorld", "test"},
	ok = notify(Table, Player, Message2).
   
