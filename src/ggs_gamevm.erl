-module(ggs_vm_runner).
-export([start_link/0, define/2, user_command/4]).
%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

%% @doc Create a new VM process. The process ID is returned and can be used
%% with for example the define method of this module.
start_link() ->  
    erlang_js:start(), %% @TODO: should only be done once
    PortPid = spawn_link(   fun() ->
                                process_flag(trap_exit, true),
                                {ok, Port} = js_driver:new(),
                                js:define(Port, <<"function userCommand(user, command, args){return 'Hello world';}">>),  
                                loop(Port)
                            end ),
    PortPid. 

%% @doc Define some new code on the specified VM, returns the atom ok.
define(GameVM, SourceCode) ->
    GameVM ! {define,SourceCode},
    ok.

%% @doc Execute a user command on the specified VM. This function is
%% asynchronous, and returns ok.
%% @spec user_command(GameVM, User, Command, Args) -> ok
%%      GameVM  = process IS of VM
%%      Player  = the player running the command
%%      Command = a game command to run
%%      Args    = arguments for the Command parameter
user_command(GameVM, Player, Command, Args) ->
    Ref = make_ref(),
    GameVM ! {user_command, Player, Command, Args, self(), Ref},
    ok.

%% Helper functions

loop(Port) ->
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
