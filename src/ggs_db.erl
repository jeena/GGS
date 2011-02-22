%%%%----------------------------------------------------
%%% @author     Mattias Pettersson <mattiaspgames@gmail.com>
%%% @copyright  2011 Mattias Pettersson
%%% @doc        Database for runtime game variable storage.
%%% @end

-module(ggs_db).
-import(mnesia).
%-compile({no_auto_import,[length/2]}).
-export([init/0,setItem/4,getItem/3,removeItem/3,key/3,clear/2,clear/1,length/2]).
-include("ggs_db.hrl").

%%-----------------------------------------------------
%% Creation
%%-----------------------------------------------------
init() ->
    mnesia:create_table(data, [{attributes, record_info(fields, data)}]).

%%-----------------------------------------------------
%% Test
%%-----------------------------------------------------

%test_data() ->
%    set(0, "Hello"),
%    get(0).

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
%% Querries
%%-----------------------------------------------------
getItem(GameToken,Ns,Key) ->
    Fun = fun() -> 
                [Data] = mnesia:read(data, {GameToken,Ns,Key}),
                Data#data.value
          end,
    mnesia:transaction(Fun).
 
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


    
