%%%----------------------------------------------------
%%% @author     Jonatan Pålsson <Jonatan.p@gmail.com>
%%% @copyright  2010 Jonatan Pålsson
%%% @doc        RPC over TCP server
%%% @end
%%%----------------------------------------------------

-module(ggs_server).
-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/0,
         get_count/0,
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
%% @doc     Fetches the number of requests made to this server
%% @spec    get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%-----------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

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

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.client_vm_map}, State};
 
handle_call({backup_state, OldState}, _From, State) ->
    io:format("Received old state from backup~n"),
    {noreply, OldState}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = do_JSCall(Socket, RawData, State),
    OldMap = State#state.client_vm_map,
    io:format("Old map: ~p NewState: ~p~n", [OldMap, NewState]),
    {noreply, State#state{client_vm_map = OldMap ++ [NewState]}};

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

do_JSCall(Socket, Data, State) ->
    Parsed = ggs_protocol:parse(Data),
    NewState = case Parsed of
        {define, Token, Payload} ->
            JSVM = getJSVM(Token, State),
            js_runner:define(JSVM, Payload),
            send(Socket, Token, "Okay, defined that for you!"),
            [];
        {call, Token, Payload} ->
            io:format("Got call request: ~p~n", [Payload]),
            JSVM = getJSVM(Token, State),
            erlang:display(erlang:port_info(JSVM)),
            {ok, Ret} = js_runner:call(JSVM, Payload, []),%Payload, []),
            send(Socket, Token, "JS says:", binary_to_list(Ret));
            
        % Set the new state to the reference generated, and JSVM associated
        {hello, _, _} ->
            JSVM = js_runner:boot(), 
            Client = getRef(),
            send(Socket, Client, "This is your refID"),
            
            %% This shouldnt be here...
            OldMap = State#state.client_vm_map,
            BackupState = State#state{client_vm_map = OldMap ++ [{Client, JSVM}]},
            gen_server:cast(ggs_backup, {set_backup, BackupState}),
            %%
            
            {Client, JSVM};
        {echo, RefID, _, MSG} ->
            send(Socket, RefID, "Your VM is ", getJSVM(RefID, State)),
            [];
        {crash, Zero} ->
            10/Zero;
        {vms} ->
            send(Socket, "RefID", State);
        % Set the new state to []
        Other ->
            send(Socket, "RefID", "__error"),
            []
    end,
    % Return the new state
    NewState.
%%-----------------------------------------------------
%% Helpers 
%%-----------------------------------------------------
getRef() ->
    %{A1,A2,A3} = now(),
    %#random:seed(A1, A2, A3),
    %random:uniform(1000).
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
