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

-record(state, {port, lsock, request_count = 0}).

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
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    do_JSDefine(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};

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

do_JSDefine(Socket, Data) ->
    JSVM = js_runner:boot(), 
    js_runner:define(JSVM, "function userCommand(cmd, par) {return cmd+' '+ par}"),
    Parsed = ggs_protocol:parse(Data),
    case Parsed of
        {cmd, Command, Parameter} ->
            Ret = js_runner:call(JSVM, "userCommand", 
                [list_to_binary(Command), 
                 list_to_binary(Parameter)]),
            send(Socket, "JS says: ", Ret);
        {hello} ->
            io:format("Got hello!"),
            send(Socket, make_ref(), "__ok_hello");
        Other ->
            io:format("Got '~p'", [Other]),
            send(Socket, "__error")
    end.

do_JSCall(Socket, Function, Parameters) ->
    ok.

send(Socket, String) ->
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [String])).

send(Socket, String1, String2) ->
    gen_tcp:send(Socket, io_lib:fwrite("~p ~p~n", [String1, String2])).
