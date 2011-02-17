-module(ggs_coordinator_test).
-include_lib("eunit/include/eunit.hrl").

coordinator_test_() ->
    {foreach,
         fun() -> 
                {ok, _Coord} = ggs_coordinator:start_link(),
                timer:sleep(100)
            end,
         fun(_X) ->   
                ggs_coordinator:stop("End of test"), 
                timer:sleep(100)
            end,
        [
            fun test_start_link/0,
            fun test_stop/0,
            fun test_join_bad_table/0,
            fun test_create_table/0
        ]
    }.

test_start_link() ->
    % Check process info
    PInfo = whereis(ggs_coordinator), 
    ?assert((PInfo /= undefined)).                % Did the server start?

test_stop() ->
    ok = ggs_coordinator:stop(""),              % Extra cleaning
    timer:sleep(100), 
    % Did it stop?
    ?assert((whereis(ggs_coordinator)) == undefined). 

test_join_bad_table() ->
    Response = ggs_coordinator:join_table("Nonexistant table"),
    ?assert(Response == {error, no_such_table}).


test_create_table() ->
    % Forcibly create a table. This functionality should be disabled
    % in the production system, but is pretty nice for testing.
    Response = ggs_coordinator:create_table({force, 1337}),
    ?assert(Response == {ok, 1337}).
