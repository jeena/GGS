-module(ggs_network_eqc_test).

-include_lib("../../lib/eqc/include/eqc.hrl").

-compile(export_all).


start() ->
    {ok, ListenSocket} = listen(),
    register(listen, ListenSocket),
    Pid = spawn(fun() -> {ok, AcceptSocket} = accept(ListenSocket),
          
          receive {From, accept} -> From!{AcceptSocket} end end),
    {ok, ConnectSocket} = ggs_network:connect(),
    Pid!{self(), accept},
    receive 
        {AcceptSocket} ->
            io:format("AcceptSocket created~n"),
            %eqc:quickcheck(prop_connect()),
            %eqc:quickcheck(prop_receive_content(AcceptSocket, ConnectSocket))
            prop_receive_content(AcceptSocket, ConnectSocket)
    end.

prop_connect() ->
    {Atom, _} = ggs_network:connect(),
    eqc:equals(Atom, ok).

prop_receive_content(AcceptSocket, ConnectSocket) ->
    %{ok, ConnectSocket} = ggs_network:connect(),
    io:format("Connected~n"),
    %spawn(fun() -> {ok, B} = gen_tcp:recv(ConnectSocket, 1), io:format("Data: ~s~n",[B]) end),
    %timer:sleep(300),
    Pid = spawn(fun() -> C = receive_content(ConnectSocket), 
                 io:format("Content: ~s~n", [C]) end),
    
    io:format("Before send to AcceptSocket~n"),
    gen_tcp:send(AcceptSocket, "Hello\n\n"),
    io:format("After send to AcceptSocket~n"),
    eqc:equals(ok,ok).

receive_content(Socket) ->
    receive_content_(0, "", Socket).
        
receive_content_(Amount, Headers, Socket) ->
    {ok, Char1} = gen_tcp:recv(Socket, 1), 
    case Char1 of
        "\n" -> case Amount of
                    1 -> Headers;
                    _ -> receive_content_(Amount + 1,
                                         Headers ++ Char1, 
                                          Socket)
              end;
      _ -> receive_content_(0, Headers ++ Char1, Socket)
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


