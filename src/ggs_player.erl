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

-export([start/0, stop/1, notify/3, notify_game/2, get_token/1, save_socket/2]).

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
start() -> 
    gen_server:start(?MODULE, [], []).

init([]) ->
    {ok, Protocol} = ggs_protocol:start_link(),
    {ok, Token} = ggs_coordinator:join_lobby(),

    State = #state{
        token = Token,
        protocol = Protocol
    },
    {ok, State}.
    
save_socket(Player, Socket) ->
    gen_server:cast(Player, {save_socket, Socket}).

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

handle_cast({save_socket, Socket}, State) ->
    {noreply, State#state { socket = Socket } };
handle_cast({tcp, _Socket, Data}, #state { protocol = Protocol } = State) ->
    ggs_protocol:parse(Protocol, Data),
    {noreply, State};
handle_cast({notify, Message}, #state { socket = Socket } = State) ->
    gen_tcp:send(Socket, ggs_protocol:create_message(Message)),
    {noreply, State};    
handle_cast({srv_cmd, "hello", _Headers, TableToken}, State) ->
    Table = case TableToken of
        "" ->
            case ggs_coordinator:create_table() of
                {ok, NewTableToken, TablePid} ->
                    ggs_coordinator:join_table(NewTableToken),
                    {ok, NewTableToken, TablePid};
                E ->
                    E
            end;
        _  ->
            ggs_coordinator:join_table(TableToken)
    end,
    erlang:display(Table),
    case Table of
        {error, Error} ->
            ggs_player:notify(self(), self(), {"error", atom_to_list(Error)}),
            {noreply, State};
        {ok, TT, TPid} ->
            ShallDefine = case ggs_table:already_defined(TPid) of
               true -> "true";
               false -> "false"
            end,
            ggs_player:notify(self(), self(), {"hello", State#state.token ++ "," ++ ShallDefine ++ "," ++ TT}),
            {noreply, State#state{ table = TPid } }
    end;
handle_cast({srv_cmd, "define", _Headers, Data}, #state { table = Table } = State) ->
    ggs_table:notify(Table, self(), {server, define, Data}),
    {noreply, State};
handle_cast({game_cmd, Command, _Headers, Data}, #state { table = Table } = State) ->
    ggs_table:notify(Table, self(), {game, Command, Data}),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, St) ->
    {stop, unimplemented1, St}.

handle_info({tcp, _Socket, Data}, #state { protocol = Protocol } = State) ->
    ggs_protocol:parse(Protocol, Data),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    erlang:display("Client disconnected, but THIS IS NOT SUPPORTED YET!~n"),
    gen_server:cast(self(), stop),
    {noreply, State}.

terminate(Reason, State) -> 
    erlang:display(Reason),
    ggs_protocol:stop(State#state.protocol),
    %ggs_table:remove_player(State#state.table, self()),
    gen_tcp:close(State#state.socket),
    % ggs_coordinator:remove_player(self(), self()), % not implemented yet
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
