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

join_table(Num) ->
    case ggs_coordinator:join_table(integer_to_list(Num)) of
        {ok, T} ->
            %io:format("Joining existing table: ~p~n", [T]),
            T;
        {error, no_such_table} ->
            case ggs_coordinator:create_table({force, integer_to_list(Num)}) of
                {ok, TBToken} -> ok
            end,
            case ggs_coordinator:join_table(integer_to_list(Num)) of
                {ok, T} -> %io:format("Creating new table: ~p~n", [T]),
                           T;
                {error, E} ->   %erlang:display(E),
                                join_table(Num+1)
            end;
        {error, table_full} ->
            %erlang:display("Table full!"),
            join_table(Num+1)
    end.

init([Socket]) ->
    {ok, Protocol} = ggs_protocol:start_link(),
    {ok, Token} = ggs_coordinator:join_lobby(),
    
    erlang:port_connect(Socket, self()),

    Table = join_table(1),
    State = #state{
        token = Token,
        socket = Socket,
        table = Table,
        protocol = Protocol
    },
    
    %ggs_protocol:parse(Protocol, Data),
    ggs_player:notify(self(), self(), {"hello", Token}), % send hello to the client
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
handle_call(_Request, _From, St) -> {stop, unimplemented, St}.

handle_cast({tcp, _Socket, Data}, #state { protocol = Protocol } = _State) ->
    ggs_protocol:parse(Protocol, Data);

handle_cast({tcp_closed, _Socket}, _State) ->
    erlang:display("Client disconnected, but THIS IS NOT SUPPORTED YET!~n");

handle_cast({notify, Message}, #state { socket = Socket } = State) ->
    gen_tcp:send(Socket, ggs_protocol:create_message(Message)),
    {noreply, State};    

handle_cast({srv_cmd, "hello", _Headers, Data}, #state { token = Token } = State) ->
    ggs_player:notify(self(), self(), {"hello", Token}),
    {noreply, State};

handle_cast({srv_cmd, "define", _Headers, Data}, #state { table = Table } = State) ->
    ggs_table:notify(Table, self(), {server, define, Data}),
    {noreply, State};

handle_cast({game_cmd, Command, _Headers, Data}, #state { table = Table } = State) ->
    ggs_stats:message(),
    ggs_table:notify(Table, self(), {game, Command, Data}),
    {noreply, State};

handle_cast(Request, St) ->
    {stop, unimplemented1, St}.

handle_info({tcp, _Socket, Data}, #state { protocol = Protocol } = State) ->
    ggs_protocol:parse(Protocol, Data),
    {noreply, State}.

terminate(Reason, State) -> 
    erlang:display(Reason),
    ggs_table:remove_player(State#state.table, self()),
    % ggs_coordinator:remove_player(self(), self()), % not implemented yet
    % TODO: release Socket
    ok.

code_change(_OldVsn, St, _Extra) -> {ok, St}.
