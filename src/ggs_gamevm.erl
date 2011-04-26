%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

-module(ggs_gamevm).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { port, table, global } ).

%% API
-export([start_link/1, define/3, player_command/5, stop/1, call_js/2]).


%% ----------------------------------------------------------------------
% API implementation

%% @doc Create a new VM process. The process ID is returned and can be used
%% with for example the define method of this module.
start_link(Table) ->
    %erlv8_vm:start(),   %c NEW
    application:start(erlv8),
    %erlang_js:start(), %c OLD %% @TODO: should only be done once
    {ok, Pid} = gen_server:start_link(?MODULE, [Table], []),
    Pid.

%% @doc Define some new code on the specified VM, returns the atom ok.
define(GameVM, Key, SourceCode) ->
    gen_server:cast(GameVM, {define, Key, SourceCode}). %c Mod

%% @doc Execute a player command on the specified VM. This function is
%% asynchronous, and returns ok.
%% @spec player_command(GameVM, User, Command, Args) -> ok
%%      GameVM  = process IS of VM
%%      Player  = the player running the command
%%      Command = a game command to run
%%      Args    = arguments for the Command parameter
player_command(GameVM, Key, Player, Command, Args) ->
    gen_server:cast(GameVM, {player_command, Key, Player, Command, Args}).

%% @private
% only for tests
call_js(GameVM, SourceCode) ->
    gen_server:call(GameVM, {eval, SourceCode}).
    
% @doc stops the gamevm process
stop(GameVM) ->
    gen_server:cast(GameVM, stop).


%% ----------------------------------------------------------------------

%% @private
init([Table]) ->
    process_flag(trap_exit, true),
    %{ok, Port} = js_driver:new(), c Old
    {ok, Port} = erlv8_vm:start(), %c New
    Global = erlv8_vm:global(Port), %c New
	{ok, JSAPISourceCode} = file:read_file("src/ggs_api.js"),
	%ok = js:define(Port, JSAPISourceCode), c Old
    Global:set_value("src", JSAPISourceCode), %c New
	InitGGSJSString = "var GGS = new _GGS(" ++ Table ++ ");",
	%ok = js:define(Port, list_to_binary(InitGGSJSString)), c Old
    Global:set_value("ggs", InitGGSJSString), %c New
    {ok, #state { port = Port, table = Table, global = Global }}. %c Mod

%% private
% only needed for the tests
handle_call({eval, SourceCode}, _From, #state { port = Port } = State) ->
    %{ok, Ret} = js:eval(Port, list_to_binary(SourceCode)), c Old
    {ok, Ret} = erlv8_vm:run(Port, SourceCode), %c New
    {reply, Ret, State}.

%% @private    
handle_cast({define, Key, SourceCode}, #state { port = Port, table = Table,  global = Global } = State) -> %c Mod
    %Ret = js:define(Port, list_to_binary(SourceCode)), %c old
    Global:set_value(Key, SourceCode),
    ggs_table:notify_all_players(Table, {"defined", "ok"}), %c ok
    {noreply, State};  %c New
	%case Ret of %c Old
	%	ok ->
	%		ggs_table:notify_all_players(Table, {"defined", "ok"}),
	%		{noreply, State};
	%	Other ->
	%		ggs_table:notify_all_players(Table, {"defined", "error " ++ Other}),
	%		{noreply, State}
	%end;
handle_cast({player_command, Key, Player, Command, Args}, #state { port = Port, global = Global } = State) ->
    %Js = list_to_binary("playerCommand(new Player('" ++ Player  ++ "'), '" ++ js_escape(Command) ++ "', '" ++ js_escape(Args) ++ "');"), %c Old
    Js = "playerCommand(new Player('" ++ Player  ++ "'), '" ++ js_escape(Command) ++ "', '" ++ js_escape(Args) ++ "');", %c New
    %%js_driver:define_js(Port, Js), %c old
    Global:set_value(Key, Js), %c new
	erlang:display(binary_to_list(Js)),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, S) ->
    error_logger:error_report([unknown_msg, Msg]),
    {noreply, S}.

%% @private
handle_info(Msg, S) ->
    error_logger:error_report([unknown_msg, Msg]),
    {noreply, S}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

js_escape(S) ->
	lists:flatmap(fun($\') -> [$\\, $\']; (X) -> [X] end, S).

