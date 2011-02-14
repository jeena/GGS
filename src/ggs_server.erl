-module(ggs_server).
-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, client_vm_map = []}).

%%%====================================================
%%% API
%%%====================================================

%%-----------------------------------------------------
%% @doc Starts the server
%% @end
%%-----------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
    start_link(?DEFAULT_PORT).

%%-----------------------------------------------------
%% @doc     Stops the server.
%% @spec    stop() -> ok
%% @end
%%-----------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%-----------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------

init([Port]) ->
    case gen_server:call(ggs_backup, get_backup) of
        {backup_state, not_initialized} ->
            {ok, LSock} = gen_tcp:listen(Port, [{active, true},
                                                {reuseaddr, true}]),
            {ok, #state{port = Port, lsock = LSock}, 0};
        {backup_state, State} ->
            {ok, LSock} = gen_tcp:listen(Port, [{active, true},
                                                {reuseaddr, true}]),
            {ok, State#state{lsock = LSock}, 0}
    end.

handle_call({backup_state, OldState}, _From, State) ->
    io:format("Received old state from backup~n"),
    {noreply, OldState}.


handle_info({tcp, Socket, RawData}, State) ->
    ggs_protocol:parse(RawData),
    {noreply, State#state{lsock = Socket}};

handle_info({tcp_closed, Socket}, State) ->
    gen_tcp:close(Socket),
    {stop, "Client closed socket", State};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};

handle_info(Other, State) ->
    erlang:display(Other).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------
%% Internal functions
%%-----------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

% Handle javascript defines
handle_cast({srv_cmd, "define", Headers, Data}, State) ->
    Token = ggs_protocol:getToken(Headers),
    JSVM = getJSVM(Token, State),
    js_runner:define(JSVM, Data),
    send(State#state.lsock, "Token", "Okay, defined that for you!"),
    {noreply, State};

% Handle javascript calls
handle_cast({srv_cmd, "call", Headers, Data}, State) ->
    Token = ggs_protocol:getToken(Headers),
    io:format("Got call request: ~p~n", [Data]),
    JSVM = getJSVM(Token, State),
    erlang:display(erlang:port_info(JSVM)),
    {ok, Ret} = js_runner:call(JSVM, Data, []),%Payload, []),
    send(State#state.lsock, Token, "JS says:", binary_to_list(Ret)),
    {noreply, State};
            
% Set the new state to the reference generated, and JSVM associated
handle_cast({srv_cmd, "hello", Headers, Data}, State) ->
    JSVM = js_runner:boot(), 
    Client = getRef(),
    send(State#state.lsock, Client, "This is your refID"),
    OldMap = State#state.client_vm_map,
    NewState = State#state{client_vm_map = OldMap ++ [{Client, JSVM}]},
    gen_server:cast(ggs_backup, {set_backup, NewState}),
    {noreply, NewState}.
%%-----------------------------------------------------
%% Helpers 
%%-----------------------------------------------------
getRef() ->
    string:strip(os:cmd("uuidgen"), right, $\n ).

getJSVM(RefID, State) ->
    VMs = State#state.client_vm_map,
    erlang:display(RefID),
    erlang:display(VMs),
    {value, {_,VM}} = lists:keysearch(RefID, 1, VMs),
    VM.

send(Socket, RefID, String) ->
    gen_tcp:send(Socket, string:join([RefID,String,"\n"], " ")).

send(Socket, RefID, String1, String2) ->
    gen_tcp:send(Socket, string:join([RefID,String1, String2,"\n"], " ")).
