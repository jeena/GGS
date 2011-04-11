%% @doc This module handles communication between a player and GGS. This module is
%% responsible for: 
%%  * The storage of the player socket, player token and a table token.
%%  * Ability to fetch a player token.
%%  * Forwarding messages from players to the game
%%  * Remove a player from GGS

-module(ggs_player).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/1, notify/3, notify_game/2, get_token/1, stop/1]).

-vsn(1.0).

-record(state, {
    token,
    socket,
    table,
    protocol }).

%% @doc Spawns a process representing the player in GGS. Takes the player socket as
%% an argument for storage and later usage. Creates a unique player token
%% identifying the player.
%% @spec start_link(Socket::socket()) -> {ok, Pid} | {error, Reason}
start(Socket) -> 
    gen_server:start(?MODULE, [Socket], []).

init([Socket]) ->
    {ok, Protocol} = ggs_protocol:start_link(),
    {ok, Token} = ggs_coordinator:join_lobby(),

    case ggs_coordinator:join_table("1337") of
        {ok, T} ->
            Table = T;
        {error, no_such_table} ->
            ggs_coordinator:create_table({force, "1337"}),
            {ok, T} = ggs_coordinator:join_table("1337"),
            Table = T
    end,
    
    State = #state{
        token = Token,
        socket = Socket,
        table = Table,
        protocol = Protocol
    },
    erlang:display(State),
    {ok, State}.

%% @doc Handles incoming messages from the GGS and forwards them through the player
%% socket to the player.
%% @spec notify(Player::Pid(), From::Pid(), 
%%              {Command::String(), Message::string()}) -> ok
notify(Player, _From, Message) ->
    gen_server:cast(Player, {notify, Message}).

%% @doc Handles incomming messages form a client and forwards them
%% through to the game_vm
notify_game(Player, Message) ->
    gen_server:cast(Player, Message).

%% @doc Get the player token uniquely representing the player.
%% @spec get_token() -> string()
get_token(_Player) ->
    ggs_logger:not_implemented().

%% @doc Properly terminates the player process. The player token will be lost
%% together with the table token. It should also close the player socket and the 
%% process should return in the end.
%% @spec stop(Table::pid()) -> Reason::string()
stop(Player) ->
    gen_server:cast(Player, stop).

%% Internals
handle_call({notify, Message}, _From, #state { protocol = Protocol, socket = Socket } = State) ->
    gen_tcp:send(Socket, ggs_protocol:create_message(Protocol, Message)),
    {noreply, State};    

handle_call({game_cmd, Command, _Headers, Data}, _From, #state { table = Table } = State) ->
    ggs_table:notify(Table, self(), {game, Command, Data}),
    {noreply, State};

handle_call({srv_cmd, "define", _Headers, Data}, _From, #state { table = Table } = State) ->
    ggs_table:notify(Table, self(), {server, define, Data}),
    {noreply, State};


handle_call(_Request, _From, St) -> {stop, unimplemented, St}.

handle_cast({tcp, _Socket, Data}, #state { protocol = Protocol } = _State) ->
    ggs_protocol:parse(Protocol, Data);

handle_cast(_Request, St) -> {stop, unimplemented, St}.

handle_info(_Info, St) -> {stop, unimplemented, St}.

terminate(_Reason, State) -> 
    ggs_protocol:stop(State#state.protocol),
    ggs_table:remove_player(State#state.table, self()),
    % ggs_coordinator:remove_player(self(), self()), % not implemented yet
    % TODO: release Socket
    ok.

code_change(_OldVsn, St, _Extra) -> {ok, St}.
