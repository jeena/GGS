%%%----------------------------------------------------
%%% @author     Jonatan Pålsson <Jonatan.p@gmail.com>
%%% @copyright  2010 Jonatan Pålsson
%%% @end
%%%----------------------------------------------------

-module(ggs_server).

% import
-import(ggs_network).

%% API
-export([start/0,
         start/1,
         get_count/0,
         stop/0
         ]).



%%%====================================================
%%% API
%%%====================================================

%%-----------------------------------------------------
%% @doc Starts the server
%% @end
%%-----------------------------------------------------
start() ->
    ggs_network:start_link().

start(Port) ->
    ggs_network:start_link(Port).


%%-----------------------------------------------------
%% @doc     Fetches the number of requests made to this server
%% @spec    get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%-----------------------------------------------------
<<<<<<< HEAD
get_count() ->
    ggs_network:get_count(get_count).
=======
get_count() -> gen_server:call(?SERVER, get_count).
_crash()    -> gen_server:call(?SERVER, _crash).
_vms() 	    -> gen_server:call(?SERVER, _vms).
_hello()    -> gen_server:call(?SERVER, _hello).
_echo()     -> gen_server:call(?SERVER, {_echo, RefID, _, MSG}).
>>>>>>> 02b72e54457112cb8d576cdcf3e982d2e34407fc

%%-----------------------------------------------------
%% @doc     Stops the server.
%% @spec    stop() -> ok
%% @end
%%-----------------------------------------------------
stop() ->
    ggs_network:stop().

%%-----------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true},
                                        {reuseaddr, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.client_vm_map}, State};

handle_call(_crash, _From, State) -> 
	Zero/10.
	{reply, sdas , State};

%handle_call(_hello, _From, State) ->
% Client = getRef();
% send(Socket, Client, "_ok_hello"),
%   {Client, JVSM)
%{reply, Client, State};

%handle_call(_vms, _From, State) ->
% send(Socket, "RefID", State)
%{reply, , State};

%handle_call(_echo, RefID, _, MSG) ->
%{reply, ,State};  

handle_call(_Message, _From, State)
	{reply, error, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = do_JSCall(Socket, RawData, State),
    OldMap = State#state.client_vm_map,
    io:format("Old map: ~p NewState: ~p~n", [OldMap, NewState]),
    {noreply, State#state{client_vm_map = OldMap ++ [NewState]}};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

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

getJSVM(RefID, State) ->
    VMs = State#state.client_vm_map,
    {value, {_,VM}} = lists:keysearch(RefID, 1, VMs),
    VM.
>>>>>>> 02b72e54457112cb8d576cdcf3e982d2e34407fc
