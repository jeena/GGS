-module(ggs_vm_runner).
-export([start_link/0, define/2, user_command/4]).

%Mattias
start_link() ->  
    erlang_js:start(),
    {ok, Port} = js_driver:new(),
    js:define(Port, <<"function userCommand(user, command, args){}">>),  
    PortPid = spawn_link(fun() -> loop(Port) end ),
    PortPid. 


loop(Port) ->
receive
    {define, SourceCode} ->
        ok = js:define(Port, list_to_binary(SourceCode)),
        loop(Port); 
    {user_command, User, Command, Args} -> 
        {ok, Ret} = js:call(Port, <<"userCommand">>, list_to_binary([User,Command,Args])),
        loop(Port)
end.
    

define(GameVM, SourceCode) ->
    GameVM ! {define,SourceCode}.

user_command(GameVM, User, Command, Args) ->
    GameVM ! {user_command, User, Command, Args}.
