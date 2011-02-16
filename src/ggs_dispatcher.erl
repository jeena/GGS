-module(ggs_dispatcher).

-behaviour(gen_server).

%% API Exports
-export([start_link/1, stop/1]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).

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
stop(_Reason) -> not_implemented.


%% gen_server callbacks

%% @doc Initiate the dispatcher. This is called from gen_server
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true},
                                        {reuseaddr, true}]),
    {ok, LSock, 0}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, RawData}, State) ->
    io:format("Got connect request!~n"),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    gen_tcp:close(Socket),
    {stop, "Client closed socket", State};

%% @doc This is our function for accepting connections. When a client connects,
%% it will immediately time out due to timing settings set in init and here,
%% and when it does, we accept the connection.
handle_info(timeout, LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(ggs_player, start_link, [Sock]),
    {noreply, LSock, 0}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
