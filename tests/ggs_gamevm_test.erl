-module(ggs_gamevm_test).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    erlang_js:start(), %% @TODO: should only be done once
	GameVM = ggs_gamevm:start_link(test_table),
	?assertNot(GameVM =:= undefined).
	
define_test() ->
    GameVM = ggs_gamevm:start_link(test_table),
    ggs_gamevm:define(GameVM, "function hello(test) { return test; }"),
    ?assertMatch(<<"jeena">>, gen_server:call(GameVM, {eval, "hello('jeena')"})).
    
stop_test() ->
	GameVM = ggs_gamevm:start_link(test_table),
	ok = ggs_gamevm:stop(GameVM).

user_command_test() ->
    GameVM = ggs_gamevm:start_link(test_table),
    ggs_gamevm:define(GameVM, "var t = '';\nfunction userCommand(user, command, args) { t = user + command + args; }\n"),
    ggs_gamevm:user_command(GameVM, "'jeena", "thecommand", "theargs'"),
    ?assertMatch(<<"'jeenathecommandtheargs'">>, gen_server:call(GameVM, {eval, "t;"})).

js_erlang_test() ->
    GameVM = ggs_gamevm:start_link(test_table),
    ggs_gamevm:define(GameVM, "var t = '';\nfunction userCommand(user, command, args) { t = callErlang('erlang time') + ''; }\n"),
    ggs_gamevm:user_command(GameVM, "", "", ""),
	{A, B, C} = erlang:time(),
	T = "{" ++ integer_to_list(A) ++ ", " ++ integer_to_list(B) ++ ", " ++ integer_to_list(C) ++ "}",
	?assertMatch(T, binary_to_list(gen_server:call(GameVM, {eval, "t;"}))).

