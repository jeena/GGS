-module(js_runner).
-export([boot/0]).

%Mattias
boot() ->  
    erlang_js:start(),
    {ok, Port} = js_driver:new(),
    PortPid = spawn(fun() -> port_process(Port) end ),
    PortPid. 


port_process(Port) ->
receive
    {get_port, From} -> 
        From!{ok,Port},
        port_process(Port);
    {define, From, Data} ->
        ok = js:define(From, list_to_binary(Data)),
        From!{ok},
        port_process(Port); 
    {call, From, Func, Params} -> 
        {ok,Ret} = js:call(From, list_to_binary(Func), Params), %Port unsure
        From!{ok,Ret},
        port_process(Port)
end.
    
%These two babies will be ambigiuous
%define(Port, Data) ->
%    port_pid!{define,self(),Port,Data}.
    

%call(Port, Func, Params) ->
%    port_pid!{call, self(), Port, Func, Params}.
