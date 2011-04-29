%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

-module(ggs_gamevm).
-behaviour(gen_server).

-include_lib("lib/erlv8/include/erlv8.hrl").

%% API
-export([start/0,start_link/1,stop/1]).
-export([define/3, player_command/5, call_js/2]). 

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-record(state, { vm, global, table } ).

%% Macros
-define(LOCALSTORAGE, "localstorage").
-define(WORLD, "world").
-define(PLAYER, "player").

%% ----------------------------------------------------------------------
% API implementation

start() ->
    Table = "table",
    start_link(Table).

%% @doc Create a new VM process. The process ID is returned and can be used
%% with for example the define method of this module.
start_link(Table) -> 
    {ok, AccessVM} = gen_server:start_link(?MODULE, [Table], []),
    expose_to_js(AccessVM),
    AccessVM.

%% @doc Define some new code on the specified VM, returns the atom ok.
define(AccessVM, Key, SourceCode) ->
    gen_server:cast(AccessVM, {define, Key, SourceCode}).

%% @doc Execute a player command on the specified VM. This function is
%% asynchronous, and returns ok.
%% @spec player_command(GameVM, User, Command, Args) -> ok
%%      GameVM  = process IS of VM
%%      Player  = the player running the command
%%      Command = a game command to run
%%      Args    = arguments for the Command parameter
player_command(accessVM, Key, Player, Command, Args) ->
    gen_server:cast(accessVM, {player_command, Key, Player, Command, Args}).

%% @private
% only for tests
call_js(AccessVM, SourceCode) ->
    gen_server:call(AccessVM, {eval, SourceCode}).

% @doc stops the gamevm process
stop(AccessVM) ->
    gen_server:cast(AccessVM, stop).


init([Table]) -> 
    process_flag(trap_exit, true),
    application:start(erlv8),    % Start erlv8
    {ok, VM} = erlv8_vm:start(),    % Create a JavaScript vm
    Global = erlv8_vm:global(VM),    % Retrieve JS global
    ggs_db:init(),    % Initialize the database
    {ok, #state { vm = VM, global = Global, table = Table }}.   

%% Expose to JavaScript 

expose_to_js(AccessVM) ->
    expose_localstorage_set_item(AccessVM),
    expose_localstorage_get_item(AccessVM),
    expose_localstorage_key(AccessVM),
    expose_localstorage_length(AccessVM),
    expose_localstorage_remove_item(AccessVM), 
    expose_localstorage_clear1(AccessVM), 
    expose_localstorage_clear2(AccessVM),

    expose_world_set_item(AccessVM),
    expose_world_get_item(AccessVM),
    expose_world_key(AccessVM),
    expose_world_length(AccessVM),
    expose_world_remove_item(AccessVM), 
    expose_world_clear1(AccessVM), 
    expose_world_clear2(AccessVM),

    expose_player_set_item(AccessVM),
    expose_player_get_item(AccessVM),
    expose_player_key(AccessVM),
    expose_player_length(AccessVM),
    expose_player_remove_item(AccessVM), 
     
    expose_table_send_command(AccessVM).


%% Helpers
 
expose_localstorage_set_item(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_set_item).

expose_localstorage_remove_item(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_remove_item).

expose_localstorage_clear1(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_clear1).

expose_localstorage_clear2(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_clear2).

expose_localstorage_get_item(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_get_item).
                                                                                          
expose_localstorage_length(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_length).

expose_localstorage_key(AccessVM) ->
    gen_server:cast(AccessVM, localstorage_key).

expose_world_set_item(AccessVM) ->
    gen_server:cast(AccessVM, world_set_item).

expose_world_remove_item(AccessVM) ->
    gen_server:cast(AccessVM, world_remove_item).

expose_world_clear1(AccessVM) ->
    gen_server:cast(AccessVM, world_clear1).

expose_world_clear2(AccessVM) ->
    gen_server:cast(AccessVM, world_clear2).

expose_world_get_item(AccessVM) ->
    gen_server:cast(AccessVM, world_get_item).
                                                                                          
expose_world_length(AccessVM) ->
    gen_server:cast(AccessVM, world_length).

expose_world_key(AccessVM) ->
    gen_server:cast(AccessVM, world_key).

expose_player_set_item(AccessVM) ->
    gen_server:cast(AccessVM, player_set_item).

expose_player_remove_item(AccessVM) ->
    gen_server:cast(AccessVM, player_remove_item).

expose_player_get_item(AccessVM) ->
    gen_server:cast(AccessVM, player_get_item).
                                                                                          
expose_player_length(AccessVM) ->
    gen_server:cast(AccessVM, player_length).

expose_player_key(AccessVM) ->
    gen_server:cast(AccessVM, player_key).
 
%% Expose ggs_table

expose_table_send_command(AccessVM) ->
    gen_server:cast(AccessVM, send_command).


%% TODO: expose_table_send_command_to_all()


%% Expose helpers

expose_fun_localstorage(Global, Name, Fun, 1) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg]) -> 
                                                      Fun(Arg) end}]));
     
expose_fun_localstorage(Global, Name, Fun, 2) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2]) -> 
                                                      Fun(Arg1,Arg2) end}]));
 

expose_fun_localstorage(Global, Name, Fun, 3) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3]) -> 
                                                      Fun(Arg1,Arg2,Arg3) end}]));

expose_fun_localstorage(Global, Name, Fun, 4) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3,Arg4]) -> 
                                                      Fun(Arg1,Arg2,Arg3,Arg4)
                                                      end}])).

 expose_fun_world(Global, Name, Fun, 1) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg]) -> 
                                                      Fun(Arg) end}]));
     
expose_fun_world(Global, Name, Fun, 2) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2]) -> 
                                                      Fun(Arg1,Arg2) end}]));
 

expose_fun_world(Global, Name, Fun, 3) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3]) -> 
                                                      Fun(Arg1,Arg2,Arg3) end}]));

expose_fun_world(Global, Name, Fun, 4) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3,Arg4]) -> 
                                                      Fun(Arg1,Arg2,Arg3,Arg4)
                                                      end}])).
expose_fun_player(Global, Name, Fun, 1) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg]) -> 
                                                      Fun(Arg) end}]));
     
expose_fun_player(Global, Name, Fun, 2) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2]) -> 
                                                      Fun(Arg1,Arg2) end}]));
 

expose_fun_player(Global, Name, Fun, 3) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3]) -> 
                                                      Fun(Arg1,Arg2,Arg3) end}]));

expose_fun_player(Global, Name, Fun, 4) ->
     Global:set_value("GGS.world", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3,Arg4]) -> 
                                                      Fun(Arg1,Arg2,Arg3,Arg4)
                                                      end}])).
  
%% Retrieve a JavaScript file from hard drive and return it
%read_js_file(Filename) ->
%     {ok, JSApp} = file:read_file(Filename),
%     erlang:binary_to_list(JSApp).
     

%print(Args) ->
%    io:format("~s~n", [Args]).


%% private
% only needed for the tests
handle_call({eval, SourceCode}, _From, #state { vm = VM } = State) ->
    {ok, Ret} = erlv8_vm:run(VM, SourceCode), 
    {reply, Ret, State}.

handle_cast({define, Key, SourceCode}, 
            #state { table = Table,  global = Global } = State) ->
    Global:set_value(Key, SourceCode),
    ggs_table:notify_all_players(Table, {"defined", "ok"}), 
    {noreply, State};  



handle_cast({player_command, Key, Player, Command, Args}, 
            #state { global = Global } = State) ->
    Js = "playerCommand(new Player('" ++ Player  ++ "'), '" ++ 
         js_escape(Command) ++ "', '" ++ js_escape(Args) ++ "');", 
    Global:set_value(Key, Js), 
    erlang:display(binary_to_list(Js)),
    {noreply, State};

handle_cast(stop, #state { vm = VM } = State) ->
    erlv8_vm:stop(VM),
    {stop, normal, State};

 handle_cast(localstorage_set_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key,Value) -> ggs_db:setItem(GameToken,?LOCALSTORAGE,Key,Value) end),
    expose_fun_localstorage( Global, "setItem",  Fun, 3),
    {noreply, State};

handle_cast(localstorage_get_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:getItem(GameToken,?LOCALSTORAGE,Key) end),
    expose_fun_localstorage( Global, "getItem",  Fun, 2),
    {noreply, State};

handle_cast(localstorage_key, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Position) -> ggs_db:key(GameToken,?LOCALSTORAGE,Position) end),
    expose_fun_localstorage( Global, "key",  Fun, 2),
    {noreply, State};
 
handle_cast(localstorage_length, #state { global = Global } = State) ->
    Fun = (fun(GameToken) -> ggs_db:length(GameToken,?LOCALSTORAGE) end),
    expose_fun_localstorage( Global, "length",  Fun, 1),
    {noreply, State};

handle_cast(localstorage_remove_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:removeItem(GameToken,?LOCALSTORAGE,Key) end),
    expose_fun_localstorage( Global, "removeItem",  Fun, 2),
    {noreply, State};
 
handle_cast(localstorage_clear1, #state { global = Global } = State) ->
    Fun = (fun(GameToken) -> ggs_db:clear(GameToken) end),
    expose_fun_localstorage( Global, "clear",  Fun, 1),
    {noreply, State};
 
handle_cast(localstorage_clear2, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:removeItem(GameToken,?LOCALSTORAGE,Key) end),
    expose_fun_localstorage( Global, "clear",  Fun, 2),
    {noreply, State};
 
handle_cast(world_set_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key,Value) -> ggs_db:setItem(GameToken,?WORLD,Key,Value) end),
    expose_fun_world( Global, "setItem",  Fun, 3),
    {noreply, State};

handle_cast(world_get_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:getItem(GameToken,?WORLD,Key) end),
    expose_fun_world( Global, "getItem",  Fun, 2),
    {noreply, State};

handle_cast(world_key, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Position) -> ggs_db:key(GameToken,?WORLD,Position) end),
    expose_fun_world( Global, "key",  Fun, 2),
    {noreply, State};
 
handle_cast(world_length, #state { global = Global } = State) ->
    Fun = (fun(GameToken) -> ggs_db:length(GameToken,?WORLD) end),
    expose_fun_world( Global, "length",  Fun, 1),
    {noreply, State};

handle_cast(world_remove_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:removeItem(GameToken,?WORLD,Key) end),
    expose_fun_world( Global, "removeItem",  Fun, 2),
    {noreply, State};
 
handle_cast(world_clear1, #state { global = Global } = State) ->
    Fun = (fun(GameToken) -> ggs_db:clear(GameToken) end),
    expose_fun_world( Global, "clear",  Fun, 1),
    {noreply, State};
 
handle_cast(world_clear2, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:removeItem(GameToken,?WORLD,Key,Key) end),
    expose_fun_world( Global, "clear",  Fun, 2),
    {noreply, State};

handle_cast(player_set_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key,Value) -> ggs_db:setItem(GameToken,?PLAYER,Key,Value) end),
    expose_fun_player( Global, "setItem",  Fun, 3),
    {noreply, State};

handle_cast(player_get_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:getItem(GameToken,?PLAYER,Key) end),
    expose_fun_player( Global, "getItem",  Fun, 2),
    {noreply, State};

handle_cast(player_key, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Position) -> ggs_db:key(GameToken,?PLAYER,Position) end),
    expose_fun_player( Global, "key",  Fun, 2),
    {noreply, State};
 
handle_cast(player_length, #state { global = Global } = State) ->
    Fun = (fun(GameToken) -> ggs_db:length(GameToken,?PLAYER) end),
    expose_fun_player( Global, "length",  Fun, 1),
    {noreply, State};

handle_cast(player_remove_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Key) -> ggs_db:removeItem(GameToken,?PLAYER,Key) end),
    expose_fun_player( Global, "removeItem",  Fun, 2),
    {noreply, State};
 
handle_cast(send_command, #state { global = Global } = State) ->
    Fun = (fun(GameToken,PlayerToken,Message) -> ggs_table:send_command(GameToken,PlayerToken,Message) end),
    expose_fun_player( Global, "sendCommand", Fun, 3),
    {noreply, State};

handle_cast(Msg, S) ->
    error_logger:error_report([unknown_msg, Msg]),
    {noreply, S}.

%% @private
code_change(_, State, _) -> 
    {ok, State}.

%% @private
handle_info(Msg, S) ->
    error_logger:error_report([unknown_msg, Msg]),
    {noreply, S}.


%% @private
terminate(_, _) ->
    ok.

%% @private
%% This function is probably important. Do not touch!
js_escape(S) ->
    lists:flatmap(fun($\') -> [$\\, $\']; (X) -> [X] end, S).
