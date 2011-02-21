-module(ggs_db_test).
%-compile({no_auto_import,[get/1,set/2]}).
-include_lib("eunit/include/eunit.hrl").
  
%ggs_db_test_() ->
%    {spawn,
%        {setup, fun setup/0, fun cleanup/1,[ fun ggs_db_test/0 ]}
%    }.


%Key should be a tuple of two elements
getItem_setItem_test() -> 
    mnesia:start(),
    ggs_db:init(),
    ggs_db:setItem("dbname","nsname","keyname","Hello"),
    ?assertMatch({atomic,"Hello"},ggs_db:getItem("dbname","nsname","keyname")).

length_test() ->
    ggs_db:setItem(1,1,2,"112"),
    ggs_db:setItem(1,2,2,"122"),
    ggs_db:setItem(1,1,3,"113"),
    ggs_db:setItem(1,1,4,"114"),
    ?assertEqual(ggs_db:length(1,1), 3).

removeItem_test() ->
    ggs_db:removeItem(1,1,4),
    ?assertNot(ggs_db:getItem(1,1,4) =:= {atomic,"114"}).
    %mnesia:stop().

key_test() ->
    ?assert(ggs_db:key(1,1,2) =:= {1,1,3}),
    mnesia:stop().
%cleanup(Val) ->
%    mnesia:stop().



%tests() ->
%    ggs_db_test().

%ggs_db_test() ->
%    ggs_db:set(0, "Hello"),
    %io:format("~s",[Val]),
%    ?assertMatch({atomic,"Hello"},ggs_db:get(0)).

