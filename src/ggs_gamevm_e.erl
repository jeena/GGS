-module(ggs_gamevm_e).
-export([start_link/1, define/2, user_command/4]).
%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

%% @doc Create a new VM process. The process ID is returned and can be used
%% with for example the define method of this module.
start_link(Table) ->  
    PortPid = spawn(   fun() ->
                                loop(Table)
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

loop(Table) ->
    receive
        {define, SourceCode} ->
            loop(Table); 
        {user_command, _User, Command, _Args, _From, _Ref} ->
            io:format("GameVM received a message~n"),
            ggs_table:notify_all_players(Table, Command), 
            loop(Table)
    end.
