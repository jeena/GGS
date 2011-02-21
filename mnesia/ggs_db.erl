%%%%----------------------------------------------------
%%% @author     Mattias Pettersson <mattiaspgames@gmail.com>
%%% @copyright  2011 Mattias Pettersson
%%% @doc        Database for runtime game variable storage.
%%% @end

-module(ggs_db).
-import(mnesia).
%-compile({no_auto_import,[length/2]}).
-export([init/0,setItem/4,getItem/3,removeItem/3,key/3,clear/2,length/2]).
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
setItem(Db,Ns,Key,Value) ->
    Fun = fun() ->
                  Data = #data{key = {Db,Ns,Key}, value = Value},
                  mnesia:write(Data) 
          end,
    mnesia:transaction(Fun).


%%-----------------------------------------------------
%% Deletions
%%-----------------------------------------------------
removeItem(Db,Ns,Key) ->
    Fun = fun() ->
                   mnesia:delete({data,{Db,Ns,Key}})
           end,
    mnesia:transaction(Fun).


%%-----------------------------------------------------
%% Querries
%%-----------------------------------------------------
getItem(Db,Ns,Key) ->
    Fun = fun() -> 
                [Data] = mnesia:read(data, {Db,Ns,Key}),
                Data#data.value
          end,
    mnesia:transaction(Fun).
 
length(Db,Ns) ->
    Fun = fun() ->
                Keys = mnesia:all_keys(data),
                length(lists:filter(fun({A,B,_}) -> ((A==Db) and (B==Ns)) end, Keys))
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.

key(Db,Ns,Position) ->
    Fun = fun() ->
                Keys = mnesia:all_keys(data),
                Rest = lists:filter(fun({A,B,_}) -> ((A==Db) and (B==Ns)) end, Keys),
                lists:nth(Position, Rest)
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.


clear(Db,Ns) ->
    Fun = fun() ->
                 Keys = mnesia:all_keys(data),
                 Rest = lists:filter(fun({A,B,_}) -> ((A==Db) and (B==Ns)) end, Keys),
                 lists:map(fun({A,B,C}) -> removeItem(A,B,C) end, Rest)
          end,
    {atomic, Ret} = mnesia:transaction(Fun),
    Ret.
    
