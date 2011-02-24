-module(ggs_db).
-export([init/0,stop/0,setItem/4,getItem/3,removeItem/3,key/3,clear/2,clear/1,length/2]).
%-include("ggs_db.hrl").
-record(data, {key, value}).

%%-----------------------------------------------------
%% Creation
%%-----------------------------------------------------
init() -> 
%    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(data, [{attributes, record_info(fields, data)}]).

stop() -> 
    mnesia:stop().


%%-----------------------------------------------------
%% Insertions
%%-----------------------------------------------------
setItem(GameToken,Ns,Key,Value) ->
    Fun = fun() ->
                  Data = #data{key = {GameToken,Ns,Key}, value = Value},
                  mnesia:write(Data) 
          end,
    mnesia:transaction(Fun).


%%-----------------------------------------------------
%% Deletions
%%-----------------------------------------------------
removeItem(GameToken,Ns,Key) ->
    Fun = fun() ->
                   mnesia:delete({data,{GameToken,Ns,Key}})
           end,
    mnesia:transaction(Fun).

clear(GameToken,Ns) ->
    Fun = fun() ->
                 Keys = mnesia:all_keys(data),
                 Rest = lists:filter(fun({A,B,_}) -> ((A==GameToken) and (B==Ns)) end, Keys),
                 lists:map(fun({A,B,C}) -> removeItem(A,B,C) end, Rest)
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

clear(GameToken) ->
    Fun = fun() ->
                Keys = mnesia:all_keys(data),
                Rest = lists:filter(fun({A,_,_}) -> (A==GameToken) end, Keys),
                lists:map(fun({A,B,C}) -> removeItem(A,B,C) end, Rest)
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

%%-----------------------------------------------------
%% Queries
%%-----------------------------------------------------
getItem(GameToken,Ns,Key) ->
	Fun = fun() ->
		mnesia:read(data, {GameToken,Ns,Key})
	end,
	case mnesia:transaction(Fun) of
	{atomic, []} -> 
		{error};
	{atomic, [Ret]} ->
	Ret#data.value
end.	
 
length(GameToken,Ns) ->
    Fun = fun() ->
                Keys = mnesia:all_keys(data),
                length(lists:filter(fun({A,B,_}) -> ((A==GameToken) and (B==Ns)) end, Keys))
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

key(GameToken,Ns,Position) ->
    Fun = fun() ->
                Keys = mnesia:all_keys(data),
                Rest = lists:filter(fun({A,B,_}) -> ((A==GameToken) and (B==Ns)) end, Keys),
                lists:nth(Position, Rest)
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
