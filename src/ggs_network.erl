%%%----------------------------------------------------
%%% @author     Jonatan Pålsson <Jonatan.p@gmail.com>
%%% @copyright  2010 Jonatan Pålsson
%%% @doc        RPC over TCP server
%%% @end
%%%----------------------------------------------------
%%% @author     Mattias Pettersson <mattiaspgames@gmail.com>
%%% @doc        Socket module for GGS
%%% @end
%%%----------------------------------------------------


-module(ggs_network).

-behaviour(gen_server).

%define
-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).


% export
-export([start_link/0,start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_count/1,crash/0,vms/0,hello/0,echo/0]). 
-export([send/3, send/4]).
-export([stop/0]).

%% gen_server callbacks
-export([terminate/2, code_change/3]).

%state
-record(state, {port, lsock, client_vm_map = []}).


%%-----------------------------------------------------
%% @doc Starts gen_server
%% @end
%%-----------------------------------------------------
start_link() ->
    start_link(?DEFAULT_PORT).

start_link(Port) ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
 
%%-----------------------------------------------------
%% Creation
%%-----------------------------------------------------
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true},
                                        {reuseaddr, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

%%-----------------------------------------------------
%% @doc     Fetches the number of requests made to this server
%% @spec    get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%-----------------------------------------------------
get_count(get_count) ->
    gen_server:call(?SERVER, get_count).

crash()    -> gen_server:call(?server, _crash).
vms()      -> gen_server:call(?SERVER, _vms).
hello()    -> gen_server:call(?SERVER, _hello).
echo()     -> gen_server:call(?SERVER, {_echo, RefID, _, MSG}).




%%-----------------------------------------------------
%% @doc     Stops the server.
%% @spec    stop() -> ok
%% @end
%%-----------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).
 
%%-----------------------------------------------------
%% Handlers
%%-----------------------------------------------------
handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.client_vm_map}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->     %parameters coming from gen_server
    NewState = do_JSCall(Socket, RawData, State), %TODO
    OldMap = State#state.client_vm_map,
    io:format("Old map: ~p NewState: ~p~n", [OldMap, NewState]),
    {noreply, State#state{client_vm_map = OldMap ++ [NewState]}};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.


%%-----------------------------------------------------
%% TCP Calls 
%%-----------------------------------------------------
send(Socket, RefID, String) ->
    gen_tcp:send(Socket, io_lib:fwrite("~p ~p~n", [RefID,String])).

send(Socket, RefID, String1, String2) ->
    gen_tcp:send(Socket, io_lib:fwrite("~p ~p ~p~n", [RefID, String1, String2])).


%%-----------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------
%% Internal functions
%%-----------------------------------------------------
do_JSCall(Socket, Data, State) ->
    JSVM = js_runner:boot(), 
    js_runner:define(JSVM, "function userCommand(cmd, par) {return cmd+' '+ par}"),
    Parsed = ggs_protocol:parse(Data),
    NewState = case Parsed of
        {cmd, Command, Parameter} ->
        % Set the new state to []
            Ret = js_runner:call(JSVM, "userCommand", 
                [list_to_binary(Command), 
                 list_to_binary(Parameter)]),
            connection:send(Socket, "RefID", "JS says: ", Ret),
            [];
        % Set the new state to the reference generated, and JSVM associated
        {hello} ->
            Client = getRef(),
            connection:send(Socket, Client, "__ok_hello"),
            {Client, JSVM};
        {echo, RefID, _, MSG} ->
            connection:send(Socket, RefID, "Your VM is ", getJSVM(RefID, State)),
            [];
        {crash, Zero} ->
            10/Zero;
        {vms} ->
            connection:send(Socket, "RefID", State);
        % Set the new state to []
        Other ->
            ggs_connection:send(Socket, "RefID", "__error"),
            []
    end,
    % Return the new state
    NewState. 

%%-----------------------------------------------------
%% Helpers 
%%-----------------------------------------------------
getRef() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(1000).

%%-----------------------------------------------------
%% Helpers 
%%-----------------------------------------------------
getJSVM(RefID, State) ->
    VMs = State#state.client_vm_map,
    {value, {_,VM}} = lists:keysearch(RefID, 1, VMs),
    VM.
