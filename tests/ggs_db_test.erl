-module(ggs_db_test).
%-compile({no_auto_import,[get/1,set/2]}).
-include_lib("eunit/include/eunit.hrl").
  
%ggs_db_test_() ->
%    {spawn,
%        {setup, fun setup/0, fun cleanup/1,[ fun ggs_db_test/0 ]}
%    }.


%Key should be a tuple of two elements
getItem_setItem_test() ->
	ggs_db:init(),
	ggs_db:setItem("dbname","nsname","keyname1","Hello"),
	ggs_db:setItem("dbname","nsname","keyname2","Hello2"),
	ggs_db:setItem("dbname2","nsname","keyname1","Hello3"),
	ggs_db:setItem("dbname2","nsname","keyname1","Hello4"),
	ggs_db:setItem("dbname3","nsname","keyname1","Hello5"),
	"Hello" = ggs_db:getItem("dbname","nsname","keyname1").

%Test the length function of our database
length_test() ->
	ggs_db:setItem(1,1,2,"112"),
	ggs_db:setItem(1,2,2,"122"),
	ggs_db:setItem(1,1,3,"113"),
	ggs_db:setItem(1,1,4,"114"),
	?assertEqual(ggs_db:length(1,1), 3).

%Test if we can remove correctly from the database
removeItem_test() ->
	ggs_db:removeItem(1,1,4),
	?assertNot(ggs_db:getItem(1,1,4) =:= "114").

%Test the key function
key_test() ->
	?assert(ggs_db:key(1,1,2) =:= {1,1,2}).

%Test the clear function(for gametoken and ns)
clear_test() ->
	ggs_db:clear(1,1),
	?assert(ggs_db:length(1,1) =:= 0).

%Test the clear function(gametoken)
clear_GameToken_test() ->
	ggs_db:clear(1),
	?assert((ggs_db:length(1,1) + ggs_db:length(1,2)) =:= 0),
	ggs_db:stop().
