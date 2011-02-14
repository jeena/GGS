-module(ggs_vm_runner).
-export([start_link/0, define/2, user_command/4]).

%Mattias
start_link() ->  
    erlang_js:start(),
    PortPid = spawn_link(   fun() ->
                                process_flag(trap_exit, true),
                                {ok, Port} = js_driver:new(),
                                js:define(Port, <<"function userCommand(user, command, args){return 'Hello world';}">>),  
                                loop(Port)
                            end ),
    PortPid. 


loop(Port) ->
    io:format("I am PID"),
    erlang:display(self()),
    receive
        {define, SourceCode} ->
            ok = js:define(Port, list_to_binary(SourceCode)),
            loop(Port); 
        {user_command, User, Command, Args, From, Ref} -> 
            {ok, Ret} = js:call(Port, <<"userCommand">>, 
                                    [   list_to_binary(User),
                                        list_to_binary(Command),
                                        list_to_binary(Args)
                                    ]),
            From ! {Ref, Ret},
            loop(Port)
    end.
    

define(GameVM, SourceCode) ->
    GameVM ! {define,SourceCode}.

user_command(GameVM, User, Command, Args) ->
    Ref = make_ref(),
    GameVM ! {user_command, User, Command, Args, self(), Ref},
    receive 
        {Ref, RetVal} ->
            RetVal;
        Other -> Other
    end.
