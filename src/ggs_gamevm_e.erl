-module(ggs_gamevm_e).
-export([start_link/1, define/2, player_command/4]).
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

%% @doc Execute a player command on the specified VM. This function is
%% asynchronous, and returns ok.
%% @spec player_command(GameVM, User, Command, Args) -> ok
%%      GameVM  = process IS of VM
%%      Player  = the player running the command
%%      Command = a game command to run
%%      Args    = arguments for the Command parameter
player_command(GameVM, Player, Command, Args) ->
    Ref = make_ref(),
    GameVM ! {player_command, Player, Command, Args, self(), Ref},
    ok.

%% Helper functions

loop(Table) ->
    receive
        {define, _SourceCode} ->
            io:format("GameVM_e can't define functions, sorry!~n"),
            loop(Table); 
        {player_command, Player, Command, Args, _From, _Ref} ->
            erlang:display(Command),
            do_stuff(Command, Args, Player, Table),
            loop(Table)
    end.

do_stuff(Command, Args, PlayerToken, Table) ->
    case Command of
        "greet" ->
            ggs_table:notify_player(Table, PlayerToken, server, "Hello there!\n");
        "chat" ->
            Nick = ggs_db:getItem(Table, nicks, PlayerToken),
            ggs_table:notify_all_players(Table, "<"++Nick++"> "++ Args ++ "\n");
        "uname" ->
            Uname = os:cmd("uname -a"),
            ggs_table:notify_player(Table, PlayerToken, server, Uname);
        "lusers" ->
           {ok, Players} = ggs_table:get_player_list(Table),
           Nicks = lists:map(fun (P) -> ggs_db:getItem(Table, nicks, P) end, Players),
           ggs_table:notify_player(Table, PlayerToken, server,io_lib:format("LUSERS ~p\n",[Nicks]));
        "nick" ->
            ggs_db:setItem(Table,nicks,PlayerToken,Args),
            io:format("Changing nickname of ~p to ~p.", [PlayerToken, Args]);
        _Other ->
            ggs_table:notify_player(Table, PlayerToken, server, "I don't know that command..\n")
    end.
