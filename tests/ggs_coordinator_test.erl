-module(ggs_coordinator_test).
-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    {ok, Coord} = ggs_coordinator:start_link(), % Start
    PInfo = erlang:process_info(Coord),         % Check process info
    ggs_coordinator:stop(""),                   % Clean up
    timer:sleep(100),                           % Wait for cleaning..
    ?assert((PInfo /= undefined)).              % Did the server start?

stop_test() ->
    ok = ggs_coordinator:stop(""),              % Extra cleaning
    {ok, Coord} = ggs_coordinator:start_link(), % Start server
    ggs_coordinator:stop("Terminate now"),      % Send stop message
    timer:sleep(100),                           % Wait for cleaning..        
    ?assert((erlang:process_info(Coord) == undefined)). % Did it stop?
