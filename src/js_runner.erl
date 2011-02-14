-module(js_runner).
-export([boot/0,define/2,call/3]).

%Mattias
boot() ->  
    erlang_js:start(),
    {ok, Port} = js_driver:new(),
    PortPid = spawn(fun() -> 
    port_process(Port) end ),
    register(port_pid, PortPid),
    Port.


port_process(Port) ->
receive
    {get_port, From} -> 
        From!{ok,Port},
        port_process(Port);
    {define,From, JSVM, Data} ->
        ok = js:define(JSVM, list_to_binary(Data)),
        From!{ok},
        port_process(Port); 
    {call, From, JSVM, Func, Params} -> 
        {ok,Ret} = js:call(JSVM, list_to_binary(Func), Params),
        From!{ok,Ret},
        port_process(Port)
end.
    

define(Port, Data) ->
    port_pid!{define,self(),Port,Data}.
    

call(Port, Func, Params) ->
    port_pid!{call, self(), Port, Func, Params}.
