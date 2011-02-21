-module(ggs_db_test).
%-compile({no_auto_import,[get/1,set/2]}).
-include_lib("eunit/include/eunit.hrl").
  
%ggs_db_test_() ->
%    {spawn,
%        {setup, fun setup/0, fun cleanup/1,[ fun ggs_db_test/0 ]}
%    }.


%Key should be a tuple of two elements
setup_test() -> 
    mnesia:start(),
    ggs_db:init(),
    ggs_db:setItem("dbname","nsname","keyname","Hello"),
    ?assertMatch({atomic,"Hello"},ggs_db:getItem("dbname","nsname","keyname")),
    mnesia:stop().

%cleanup(Val) ->
%    mnesia:stop().



%tests() ->
%    ggs_db_test().

%ggs_db_test() ->
%    ggs_db:set(0, "Hello"),
    %io:format("~s",[Val]),
%    ?assertMatch({atomic,"Hello"},ggs_db:get(0)).

