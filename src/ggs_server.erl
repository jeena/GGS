%%%----------------------------------------------------
%%% @author     Jonatan Pålsson <Jonatan.p@gmail.com>
%%% @copyright  2010 Jonatan Pålsson
%%% @end
%%%----------------------------------------------------

-module(ggs_server).
-behaviour(gen_server).

% import
-import(ggs_connection).

%% API
-export([start_link/1,
         start_link/0,
         get_count/0,
         stop/0
         ]).

%% gen_server callbacks
-export([terminate/2, code_change/3]).


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
    process_flag(trap_exit, true),
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
