%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

-module(ggs_gamevm).
-behaviour(gen_server).

-include_lib("erlv8/include/erlv8.hrl").

%% API
-export([start_link/1,stop/1]).
-export([define/3, player_command/5, call_js/2]). 

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-record(state, { vm, global, table } ).


%% ----------------------------------------------------------------------
% API implementation


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

expose_to_js(AccessVM) ->
    expose_db_set_item(AccessVM),
    expose_db_remove_item(AccessVM), 
    expose_db_clear1(AccessVM), 
    expose_db_clear2(AccessVM),
    expose_db_get_item(AccessVM),
    expose_db_length(AccessVM),
    expose_db_key(AccessVM),
    expose_table_send_command(AccessVM).

%run() ->
%    VM = fetch("vm"),
%    JSApp = fetch("jsapp"),
%    erlv8_vm:run(VM, JSApp),
%    erlv8_vm:run(VM,"js_print('Blah!')"),
%    erlv8_vm:run(VM,"setItem('Token', 'World', 'Key', 'Value')"),
%    erlv8_vm:run(VM,"setItem('Token', 'World', 'Key2', 'Value2')"),
%    Length = ggs_db:length("Token", "World"),
%    Value = ggs_db:getItem("Token", "World", "Key"),
%    io:format("Length: ~B~n",[Length]),
%    io:format("Value: ~s~n", [Value]).
     
init([Table]) -> 
    process_flag(trap_exit, true),
    application:start(erlv8),    % Start erlv8
    {ok, VM} = erlv8_vm:start(),    % Create a JavaScript vm
    Global = erlv8_vm:global(VM),    % Retrieve JS global
    ggs_db:init(),    % Initialize the database
    {ok, #state { vm = VM, global = Global, table = Table }}.   

%%% Expose ggs_db

expose_db_set_item(AccessVM) ->
    gen_server:cast(AccessVM, set_item).

expose_db_remove_item(AccessVM) ->
    gen_server:cast(AccessVM, remove_item).

expose_db_clear1(AccessVM) ->
    gen_server:cast(AccessVM, clear1).

expose_db_clear2(AccessVM) ->
    gen_server:cast(AccessVM, clear2).

expose_db_get_item(AccessVM) ->
    gen_server:cast(AccessVM, get_item).
                                                                                          
expose_db_length(AccessVM) ->
    gen_server:cast(AccessVM, length).

expose_db_key(AccessVM) ->
    gen_server:cast(AccessVM, key).


%%% Expose ggs_table

expose_table_send_command(AccessVM) ->
    gen_server:cast(AccessVM, send_command).

%% TODO: expose_table_send_command_to_all()

%% Makes a function invokable from JavaScript as: "erlang.Name(Args)"
%expose_fun(Global, Name, Fun) ->
%    Global:set_value("erlang", erlv8_object:new([{Name, 
%                                                 fun (#erlv8_fun_invocation{},
%                                                      [Args]) -> 
%                                                      Fun(Args) end}])).

expose_fun(Global, Name, Fun, 1) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg]) -> 
                                                      Fun(Arg) end}]));
     
expose_fun(Global, Name, Fun, 2) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2]) -> 
                                                      Fun(Arg1,Arg2) end}]));
 

expose_fun(Global, Name, Fun, 3) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
                                                 fun (#erlv8_fun_invocation{},
                                                      [Arg1,Arg2,Arg3]) -> 
                                                      Fun(Arg1,Arg2,Arg3) end}]));

expose_fun(Global, Name, Fun, 4) ->
     Global:set_value("GGS.localStorage", erlv8_object:new([{Name, 
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

handle_cast(set_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Ns,Key,Value) -> ggs_db:setItem(GameToken,Ns,Key,Value) end),
    expose_fun( Global, "setItem",  Fun, 4),
    {noreply, State};

handle_cast(remove_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Ns,Key) -> ggs_db:removeItem(GameToken,Ns,Key) end),
    expose_fun( Global, "removeItem",  Fun, 3),
    {noreply, State};

handle_cast(clear1, #state { global = Global } = State) ->
    Fun = (fun(GameToken) -> ggs_db:clear(GameToken) end),
    expose_fun( Global, "clear",  Fun, 1),
    {noreply, State};

handle_cast(clear2, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Ns) -> ggs_db:clear(GameToken,Ns) end),
    expose_fun( Global, "clear",  Fun, 2),
    {noreply, State};

handle_cast(get_item, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Ns,Key) -> ggs_db:getItem(GameToken,Ns,Key) end),
    expose_fun( Global, "getItem",  Fun, 3),
    {noreply, State};

handle_cast(length, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Ns) -> ggs_db:length(GameToken,Ns) end),
    expose_fun( Global, "length",  Fun, 2),
    {noreply, State};

handle_cast(key, #state { global = Global } = State) ->
    Fun = (fun(GameToken,Ns,Position) -> ggs_db:key(GameToken,Ns,Position) end),
    expose_fun( Global, "key",  Fun, 3),
    {noreply, State};

handle_cast(send_command, #state { global = Global } = State) ->
    Fun = (fun(GameToken,PlayerToken,Message) -> ggs_table:send_command(GameToken,PlayerToken,Message) end),
    expose_fun( Global, "sendCommand", Fun, 3),
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
