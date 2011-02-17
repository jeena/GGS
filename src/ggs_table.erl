%% @doc This module represents a Player with a Socket and a Token

-module(ggs_table).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { token, players, socket, game_vm } ).

%% API
-export([start_link/2,
	 add_player/2,
	 remove_player/2,
	 stop/1,
	 notify/3]).

-include_lib("eunit/include/eunit.hrl").

%% ----------------------------------------------------------------------
% API implementation

% @doc returns a new table
start_link(Token, Socket) ->
    GameVM = ggs_gamevm:start_link(),
    {ok, Pid} = gen_server:start_link(?MODULE, [Token, Socket, GameVM], []),
    Pid.

%% @private
call(Pid, Msg) ->
    gen_server:call(Pid, Msg, infinity).

% @doc adds a player to a table
add_player(Table, Player) ->
	call(Table, {add_player, Player}).

% @doc removes player form a table
remove_player(Table, Player) ->
	call(Table, {remove_player, Player}).

% @doc stops the table process
stop(Table) ->
    gen_server:cast(Table, stop).

% @doc notifies the table with a message from a player
notify(Table, Player, Message) ->
    gen_server:cast(Table, {notify, Player, Message}).

%% ----------------------------------------------------------------------

%% @private
init([Token, Socket, GameVM]) ->
    {ok, #state { token = Token,
		  socket = Socket,
		  game_vm = GameVM,
		  players = [] }}.

%% @private
handle_call({add_player, Player}, _From, #state { players = Players } = State) ->
    {reply, ok, State#state { players = [Player | Players] }};
handle_call({remove_player, Player}, _From, #state { players = Players } = State) ->
    {reply, ok, State#state { players = Players -- [Player] }};
handle_call(get_player_list, _From, #state { players = Players } = State) ->
	{reply, {ok, Players}, State};
handle_call(Msg, _From, State) ->
    error_logger:error_report([unknown_msg, Msg]),
    {reply, ok, State}.

%% @private
handle_cast({notify, Player, Message}, #state { game_vm = GameVM } = State) ->
    case Message of
	{server, define, Args} ->
	    ggs_gamevm:define(GameVM, Args);
	{game, Command, Args} ->
	    ggs_gamevm:user_command(GameVM, Player, Command, Args)
    end,
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


%% ----------------------------------------------------------------------

% Tests

start_link_test() ->
	Table = start_link("123", none),
	?assertNot(Table =:= undefined).
	
add_player_test() ->
	Table = start_link("123", none),
	Player = test_player,
	add_player(Table, Player),
	{ok, [Player]} = gen_server:call(Table, get_player_list).
	
remove_player_test() ->
	Table = start_link("123", none),
	Player = test_player,
	Player2 = test_player2,
	add_player(Table, Player),
	{ok, [Player]} = gen_server:call(Table, get_player_list),
	add_player(Table, Player2),
	{ok, [Player2, Player]} = gen_server:call(Table, get_player_list),	
	remove_player(Table, Player),
	{ok, [Player2]} = gen_server:call(Table, get_player_list),
	remove_player(Table, Player2),
	{ok, []} = gen_server:call(Table, get_player_list).	
	
stop_test() ->
	Table = start_link("123", none),
	ok = stop(Table).

% @private
notify_test() ->
	Table = start_link("123", none),
	Player = test_player,
	Message = {server, define, "function helloWorld(x) {  }"},
	ok = notify(Table, Player, Message).
	%Message2 = {game, "helloWorld", "test"},
	%ok = notify(Table, Player, Message2).
	