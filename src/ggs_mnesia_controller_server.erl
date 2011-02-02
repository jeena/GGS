-module(ggs_mnesia_controller_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(state, {}).

%%%====================================================
%%% API
%%%====================================================

%%-----------------------------------------------------
%% @doc Starts the server
%% @end
%%-----------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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

init([]) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, {}, 0}.

handle_cast(a, State) ->
    {noreply, State}.

handle_call({getValue, Key},_From,State) ->
    {reply,value_of_key_requested_goes_here, State}.

handle_info(timeout, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------
%% Internal functions
%%-----------------------------------------------------
