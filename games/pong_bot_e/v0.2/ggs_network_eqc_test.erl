-module(ggs_network_eqc_test).

-include_lib("../../lib/eqc/include/eqc.hrl").

-compile(export_all).


start() ->
    {ok, ListenSocket} = listen(),
    register(listen, ListenSocket),
    Pid = spawn(fun() -> {ok, AcceptSocket} = accept(ListenSocket),
                gen_tcp:send(AcceptSocket, "Hello\n\n"),
                gen_tcp:close(AcceptSocket) end),
    eqc:quickcheck(prop_receive_content()),
    gen_tcp:close(ListenSocket).
    


prop_connect() ->
    {Atom, _} = ggs_network:connect(),
    eqc:equals(Atom, ok).

prop_receive_content() ->
    {ok, ConnectSocket} = ggs_network:connect(),
    Pid = spawn(fun() -> C = ggs_network:receive_content(ConnectSocket), 
                 gen_tcp:close(ConnectSocket),
                 receive
                 {From, getcontent} -> From!{C} end end ),
    Pid!{self(), getcontent},
    receive
    {C} -> eqc:equals(C,"Hello\n")
    end.

%% Helpers
listen() ->
    listen(9000).

listen(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
        {ok, LSock} ->
            {ok, LSock};
        {error, Reason} ->
            io:format("Creation of listen socket failed: ~s~n", [Reason])
    end.

recv(AcceptSocket) ->
    gen_tcp:recv(AcceptSocket, 0).

accept(ListenSocket) -> 
    case gen_tcp:accept(ListenSocket) of 
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> io:format("Error accepting listen socket~s~n", [Reason]);
        _ -> io:format("Something bad happened with accept/1~n")
    end.


%% new(length) == old(length) or 
%% new(length) == old(length) + 1
prop_setItem() ->
    ggs_db:init(),           
    F = (fun(T,N,K,V) -> ggs_db:setItem(T,N,K,V), ggs_db:length(T,N) end),
    ?FORALL({T,N,K,V},{bitstring(),bitstring(),bitstring(),bitstring()},
    (ggs_db:length(T,N) + 1 == F(T,N,K,V)) or 
    (ggs_db:length(T,N) == F(T,N,K,V))).


