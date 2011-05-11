-module(ggs_network_eqc_test).

-include_lib("../../lib/eqc/include/eqc.hrl").

-compile(export_all).


start() ->
    {ok, ListenSocket} = listen(),
    eqc:quickcheck(prop_connect()),
    gen_tcp:close(ListenSocket),

    {ok, ListenSocket2} = listen(),
    eqc:quickcheck(prop_receive_content(ListenSocket2)),
    %gen_tcp:close(ListenSocket2),
    %timer:sleep(100),

    %{ok, ListenSocket} = listen(),
    eqc:quickcheck(prop_receive_data(ListenSocket2)).


prop_connect() ->
    {Atom, Socket} = ggs_network:connect(),
    gen_tcp:close(Socket),
    eqc:equals(Atom, ok).

%% ?String++\n\n -> ok
prop_receive_content(ListenSocket) -> 
    G = fun(N) -> String = integer_to_list(N) ++"\n\n",
    accept_run_compare(String,ListenSocket, fun(X) -> ggs_network:receive_content(X) end, "\n") end,
    ?FORALL(NaturalNumber, nat(), G(NaturalNumber)).

%% old(String) == new(String) 
prop_receive_data(ListenSocket) -> 
    G = fun(N) -> String = integer_to_list(N),
                  Length = length(String),
    accept_run_compare(String,ListenSocket, fun(S) -> ggs_network:receive_data(S,Length,"") end, "") end,
    ?FORALL(NaturalNumber, nat(), G(NaturalNumber)).

%% Helpers
accept_run_compare(Arg, ListenSocket, Fun, Newline) -> 
    spawn(fun() -> {ok, AcceptSocket} = accept(ListenSocket),
    gen_tcp:send(AcceptSocket, Arg),
    gen_tcp:close(AcceptSocket) end),
 
    {ok, ConnectSocket} = ggs_network:connect(),
    Pid = spawn(fun() -> C = Fun(ConnectSocket), 
                 gen_tcp:close(ConnectSocket),
                 receive
                 {From, getcontent} -> From!{C} end end ),
    Pid!{self(), getcontent},
    receive
    {C} -> eqc:equals(C++Newline,Arg) end.

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


