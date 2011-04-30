%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

-module(ggs_gamevm).
-behaviour(gen_server).

-include_lib("lib/erlv8/include/erlv8.hrl").

%% API
-export([start/0,start_link/1,stop/1]).
-export([define/2, player_command/4, call_js/2]). 

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
    {ok, GameVM} = gen_server:start_link(?MODULE, [Table], []),
    GameVM.

%% @doc Define some new code on the specified VM, returns the atom ok.
define(GameVM, SourceCode) ->
    gen_server:cast(GameVM, {define, SourceCode}).

%% @doc Execute a player command on the specified VM. This function is
%% asynchronous, and returns ok.
%% @spec player_command(GameVM, User, Command, Args) -> ok
%%      GameVM  = process IS of VM
%%      Player  = the player running the command
%%      Command = a game command to run
%%      Args    = arguments for the Command parameter
player_command(GameVm, Player, Command, Args) ->
    gen_server:cast(GameVm, {player_command, Player, Command, Args}).

%% @private
% only for tests
call_js(GameVM, SourceCode) ->
    gen_server:call(GameVM, {eval, SourceCode}).

% @doc stops the gamevm process
stop(GameVM) ->
    gen_server:cast(GameVM, stop).


init([Table]) -> 
    process_flag(trap_exit, true),
    application:start(erlv8),    % Start erlv8
    {ok, VM} = erlv8_vm:start(),    % Create a JavaScript vm
    Global = erlv8_vm:global(VM),    % Retrieve JS global
    ggs_db:init(),    % Initialize the database
    NewGlobal = expose(Global, Table),
    {ok, #state { vm = VM, global = NewGlobal, table = Table }}.   

% @doc Exposes some GGS functions to JavaScript
expose(Global, Table) ->
    Global:set_value("GGS", erlv8_object:new([
        {"localStorage", erlv8_object:new([
            {"setItem", fun(#erlv8_fun_invocation{}, [Key, Val])-> ggs_db:setItem(Table, "localStorage", Key, Val) end},
            {"removeItem", fun(#erlv8_fun_invocation{}, [Key])-> ggs_db:removeItem(Table, "localStorage", Key) end},
            {"clear", fun(#erlv8_fun_invocation{}, [])-> ggs_db:clear(Table, "localStorage") end},            
            {"getItem", fun(#erlv8_fun_invocation{}, [Key])-> ggs_db:getItem(Table, "localStorage", Key) end},
            {"length", fun(#erlv8_fun_invocation{}, [])-> ggs_db:length(Table, "localStorage") end},
            {"key", fun(#erlv8_fun_invocation{}, [Position])-> ggs_db:key(Table, "localStorage", Position) end}
        ])},
        {"world", erlv8_object:new([
            {"setItem", fun(#erlv8_fun_invocation{}, [Key, Val])-> ggs_db:setItem(Table, "world", Key, Val) end},
            {"removeItem", fun(#erlv8_fun_invocation{}, [Key])-> ggs_db:removeItem(Table, "world", Key) end},
            {"clear", fun(#erlv8_fun_invocation{}, [])-> ggs_db:clear(Table, "world") end},            
            {"getItem", fun(#erlv8_fun_invocation{}, [Key])-> ggs_db:getItem(Table, "world", Key) end},
            {"length", fun(#erlv8_fun_invocation{}, [])-> ggs_db:length(Table, "world") end},
            {"key", fun(#erlv8_fun_invocation{}, [Position])-> ggs_db:key(Table, "world", Position) end}
        ])},
        {"sendCommand", fun(#erlv8_fun_invocation{}, [Player, Command, Args])->
                erlang:display(Table),
                ggs_table:send_command(Table, Player, {Command, Args})
            end},
        {"sendCommandToAll", fun(#erlv8_fun_invocation{}, [Command, Args])-> ggs_table:notify_all_players(Table, {Command, Args}) end} 
        %{"setTimeout", fund(#erlv8_fun_invocation{}, [Time, FunctionName])-> setTimeout(Time, FunctionName) end}
    ])).

%% Retrieve a JavaScript file from hard drive and return it
%read_js_file(Filename) ->
%     {ok, JSApp} = file:read_file(Filename),
%     erlang:binary_to_list(JSApp).

%% private
% only needed for the tests
handle_call({eval, SourceCode}, _From, #state { vm = VM } = State) ->
    {ok, Ret} = erlv8_vm:run(VM, SourceCode), 
    {reply, Ret, State}.

handle_cast({define, SourceCode}, #state { table = Table, vm = VM } = State) ->
    {ok, _Ret} = erlv8_vm:run(VM, SourceCode),
    ggs_table:notify_all_players(Table, {"defined", "ok"}),
    {noreply, State};  

handle_cast({player_command, Player, Command, Args}, #state { vm = VM } = State) ->
    Js = "playerCommand('" ++ Player  ++ "', '" ++ js_escape(Command) ++ "', '" ++ js_escape(Args) ++ "');",
    {ok, _Ret} = erlv8_vm:run(VM, Js),
    {noreply, State};

handle_cast(stop, #state { vm = VM } = State) ->
    erlv8_vm:stop(VM),
    {stop, normal, State};

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
