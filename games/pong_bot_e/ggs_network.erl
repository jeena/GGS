-module(ggs_network).
-export([connect/0,append_key_value_strings_to_dict/2,key_value_string_to_list/1]).
-export([read/2, send_command/3]).

connect() ->
    {ok,Socket} = gen_tcp:connect("ggs.jeena.net", 9000,[{active, false}]),
    %{ok,Socket} = gen_tcp:connect("localhost", 9000,[{active, false}]),
    Socket.

read(Socket, Ref) ->
    Content = receive_content(Socket),
    Headers = extract_headers(Content),
    ContentSize = dict:fetch("Content-Size", Headers), 
    ContentSizeI = list_to_integer(lists:nth(1, ContentSize)),
    Data = receive_data(Socket, ContentSizeI, ""),
    %io:format("Headers: ~s~n", [Content]),
    %io:format("Data: ~s~n", [Data]),
    received_command(Headers, Data, Ref).
    
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
                                        
receive_data(_, 0, Headers) ->
    Headers;
receive_data(Socket, ContentSize, Headers) ->
    {ok, Char} = gen_tcp:recv(Socket, 1),
    receive_data(Socket, ContentSize - 1, Headers ++ Char).

received_command(Headers, Data, Ref) ->
    {ok, CommandList} = dict:find("Client-Command", Headers),
    Command = lists:nth(1, CommandList), 
    case Command of
        "hello" ->
            pong_bot:set_game_token(Data, Ref),
            send_command("ready", "", Ref);
        "defined" -> 
            ok;
        Command ->
            pong_bot:ggsNetworkReceivedCommandWithArgs(Command,  Data, Ref)
    end.

make_message(ServerOrGame, Command, Args, Ref) ->
    GameToken = pong_bot:get_game_token(Ref),
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

send_command(Command, Args, Ref) ->
    write(make_message("Game", Command, Args, Ref), Ref).
    
write(Message, Ref) ->
    Socket = gen_server:call({global, {pong_bot, Ref}}, socket),
    gen_tcp:send(Socket, Message).

list_concat([],Ret) ->
    Ret;
list_concat([E|ES],Ret) ->
    NewRet = string:concat(Ret,E),
    list_concat(ES,NewRet).

%%%Packet parsing.%%%


extract_headers(Source) -> 
    HeaderList = string:tokens(Source, "\n"),
    key_value_strings_to_dict(HeaderList).

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


