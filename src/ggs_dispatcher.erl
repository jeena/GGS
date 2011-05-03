-module(ggs_dispatcher).

-behaviour(gen_server).

%% API Exports
-export([start_link/1, stop/1]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3, accept_loop/1]).

-define(SERVER, ?MODULE).


%% @doc This module is the entry-point for clients connecting to GGS. This is
%% the module responsible for:
%%  * Greeting a connecting client, and associating a socket for it
%%  * Spawning a ggs_player for the connecting client, passing the socket

%% @doc Starts a new dispatcher with the specified port. Registers this 
%% dispatcher under the name "ggs_dispatcher". The pid of the dispatcher
%% is returned.
%% @spec start_link(Port) -> Pid
%%      Port = Integer
%%      Pid = #<Pid>
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Stops the dispatcher with the specified reason.
%% @spec stop(Reason) -> ok.
%%      Reason = String
stop(_Reason) -> ggs_logger:not_implemented().

%% gen_server callbacks

%% @doc Initiate the dispatcher. This is called from gen_server
init([Port]) ->
    case gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]) of
        {ok, LSock} ->
            {ok, accept(LSock), 0};
        {error, Reason} ->
                {stop, Reason}
    end.
    
handle_cast({accepted, _Pid}, State) ->
    {noreply, accept(State)}.
    
accept_loop({Server, LSocket}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    % Let the server spawn a new process and replace this loop
    % with the echo loop, to avoid blocking 
    gen_server:cast(Server, {accepted, self()}),
    {ok, Player} = ggs_player:start(),
    gen_tcp:controlling_process(Socket, Player),
    ggs_player:save_socket(Player, Socket).
    
% To be more robust we should be using spawn_link and trapping exits
accept(LSocket) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket}]),
    LSocket.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.