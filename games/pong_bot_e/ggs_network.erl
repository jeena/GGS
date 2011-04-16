-module(ggs_network).
-export([connect/0,append_key_value_strings_to_dict/2,key_value_string_to_list/1]).

connect() ->
    {ok,Socket} = gen_tcp:connect("localhost", 9000,[{active, false}]),
    A = gen_tcp:recv(Socket,0),
    read(A),
    Socket.
    
read(Message) ->
    case Message of
    {ok, M} ->
        HeaderList = string:tokens(M, "\n"),
        Headers = extract_headers(HeaderList),
        Data    = extract_data(HeaderList),
        received_command(Headers, Data)
    end.

received_command(Headers, Data) ->
    {ok, CommandList} = dict:find("Client-Command", Headers),
    Command = lists:nth(1, CommandList), 
    case Command of
        "hello" ->
            io:format("Received command 'hello'~n"),
            pong_bot:set_game_token(Data),
            %gen_server:cast({global, pong_bot}, {game_token, Data}),
            send_command("Ready", "");
            %pong_bot:ggsNetworkReady(); Unneccessary
        "defined" -> 
            ok;
            %pong_bot:ggsNetworkDefined(); Unneccessary
        Command ->
            gen_server:ggsNetworkReceivedCommandWithArgs(Command,  Data)
    end.

make_message(ServerOrGame, Command, Args) ->
    io:format("Make message~n"),
    GameToken = pong_bot:get_game_token(),
%    GameToken = gen_server:call({global, pong_bot}, game_token),
    io:format("Make message2~n"),
    StrGameToken = string:concat("Token: ", GameToken),
    StrGameTokenln = string:concat(StrGameToken, "\n"),
    StrCommand = string:concat("-Command: ", Command),
    StrCommandln = string:concat(StrCommand, "\n"),
    StrFullCommand = string:concat(ServerOrGame, StrCommandln),
    StrContentLength = string:concat("Content-Length: ", integer_to_list(length(Args))),
    StrContentLengthln = string:concat(StrContentLength, "\n\n"),
    StrTokenCommand = string:concat(StrGameTokenln, StrFullCommand),
    Message = string:concat(StrTokenCommand, StrContentLengthln),

    MessageWithArgs = string:concat(Message, list_concat(Args,[])),
    MessageWithArgs.

%define(SourceCode) ->
%    write(make_message("Server", "define", SourceCode)).


send_command(Command, Args) ->
    write(make_message("Client", Command, Args)).
    
write(Message) ->
    Socket = gen_server:call({global, pong_bot}, socket),
    gen_tcp:send(Socket, Message).

list_concat([],Ret) ->
    Ret;
list_concat([E|ES],Ret) ->
    NewRet = string:concat(Ret,E),
    list_concat(ES,NewRet).
    


%%%Packet parsing.%%%


extract_headers(Source) -> 
    key_value_strings_to_dict(Source).


extract_data([]) ->
    [];
extract_data([E|ES]) ->
    KeyValueList = key_value_string_to_list(E),
    case length(KeyValueList) of
        2 ->
            extract_data(ES);
        _ ->
            E
    end.


%%%Low-level internals.%%%


%%["K1: V1","K2: V2","KN: VN" ...] -> Dict
key_value_strings_to_dict(Strings) ->
    Dict = dict:new(),
    append_key_value_strings_to_dict(Strings,Dict).

%%["K1: V1","K2: V2","KN: VN" ...], Dict -> NewDict
append_key_value_strings_to_dict([Str|Strings],Dict) ->
    KeyValueList = key_value_string_to_list(Str),
    case length(KeyValueList) of
        2  -> 
            NewDict = append_string_pair_to_dict(Dict,lists:nth(1,KeyValueList),lists:nth(2,KeyValueList)),
            append_key_value_strings_to_dict(Strings,NewDict);
        _ -> 
            append_key_value_strings_to_dict(Strings,Dict)
    end;
append_key_value_strings_to_dict([],Dict) ->
    Dict.


%%"Hello: "World!" -> ["Hello","World!"]
key_value_string_to_list(KeyValueString) ->
    string:tokens(KeyValueString, ": ").


%%Append a key str1 and a value str2 to the dict Dict
append_string_pair_to_dict(Dict, Str1, Str2) ->
    dict:append(Str1, Str2, Dict).


