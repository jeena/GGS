-module(ggs_backup).
-behaviour(gen_server).

%% API
-export([start_link/0 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(state, {port, lsock, client_vm_map = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{port = -1, lsock = -1, client_vm_map = -1}, 0}.

handle_call(get_backup, _From, State) ->
    BackedUpState = case State of
        #state{port = -1, lsock = -1, client_vm_map = -1} ->
            not_initialized;
        Other ->
            Other
    end,
    {reply, {backup_state, BackedUpState}, State}.

handle_cast({set_backup, NewState}, _State) ->
    {noreply, NewState}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
