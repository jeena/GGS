-module(ggs_stats).
-export([start_link/0, message/1, print/0, tick/0]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-vsn(0).

-record(ate, {
    server_messages = 0,
    client_messages = 0
    }).
-define(SERVER, ?MODULE).

message(Type) ->
    gen_server:cast(ggs_stats, {add_one, Type}).

print() ->
    gen_server:cast(ggs_stats, print).
    
tick() ->
    print(),
    gen_server:cast(ggs_stats, tick),
    timer:apply_after(1000, ggs_stats, tick, []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init(_Args) ->
    St = #ate{
        server_messages = 0,
        client_messages = 0
    },
    {ok, St}.

handle_cast({add_one, Type}, St) -> 
    case Type of
        server -> NewSt = #ate {
                                server_messages = St#ate.server_messages + 1,
                                client_messages = St#ate.client_messages
                                };
        client -> NewSt = #ate {
                                server_messages = St#ate.server_messages,
                                client_messages = St#ate.client_messages + 1
                               }
    end,
    {noreply, NewSt};

handle_cast(print, St) ->
    erlang:display(St#ate.count),
    {noreply, St};
    
handle_cast(tick, _St) ->
    NewSt = #ate { count = 0 },
    {noreply, NewSt}.



handle_call(_Request, _From, St) -> {stop, unimplemented, St}.
handle_info(_Info, St) -> {stop, unimplemented, St}.
terminate(_Reason, _St) -> ok.
code_change(_OldVsn, St, _Extra) -> {ok, St}.
