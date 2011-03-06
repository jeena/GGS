%% @doc This module is responsible for running the game VM:s. You can issue
%% commands to a vm using this module.

-module(ggs_gamevm).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { port, table } ).

%% API
-export([start_link/1, define/2, player_command/4, stop/1, call_js/2]).


%% ----------------------------------------------------------------------
% API implementation

%% @doc Create a new VM process. The process ID is returned and can be used
%% with for example the define method of this module.
start_link(Table) ->
    erlang_js:start(), %% @TODO: should only be done once
    {ok, Pid} = gen_server:start_link(?MODULE, [Table], []),
    Pid.

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
player_command(GameVM, Player, Command, Args) ->
    gen_server:cast(GameVM, {player_command, Player, Command, Args}).

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
    {ok, Port} = js_driver:new(),
	{ok, JSAPISourceCode} = file:read_file("src/ggs_api.js"),
	ok = js:define(Port, JSAPISourceCode),
	InitGGSJSString = "var GGS = new _GGS(" ++ Table ++ ");",
	ok = js:define(Port, list_to_binary(InitGGSJSString)),
    {ok, #state { port = Port, table = Table }}.

%% private
% only needed for the tests
handle_call({eval, SourceCode}, _From, #state { port = Port } = State) ->
    {ok, Ret} = js:eval(Port, list_to_binary(SourceCode)),
    {reply, Ret, State}.

%% @private    
handle_cast({define, SourceCode}, #state { port = Port, table = Table } = State) ->
    Ret = js:define(Port, list_to_binary(SourceCode)),
	case Ret of
		ok ->
			ggs_table:notify_all_players(Table, {"defined", "ok"}),
			{noreply, State};
		Other ->
			ggs_table:notify_all_players(Table, {"defined", "error " ++ Other}),
			{noreply, State}
	end;
handle_cast({player_command, Player, Command, Args}, #state { port = Port } = State) ->
    Js = list_to_binary("playerCommand(new Player('" ++ Player  ++ "'), '" ++ js_escape(Command) ++ "', '" ++ js_escape(Args) ++ "');"),
    js_driver:define_js(Port, Js),
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

