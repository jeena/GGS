-module(ggs_protocol).
-export([parse/1, getToken/1, create_message/4]).

%% API Functions
parse(Data) ->
    Parsed = do_parse(Data, []),
    prettify(Parsed).

getToken(Parsed) ->
    case lists:keyfind(token, 1, Parsed) of
        {_, Value} ->
            Value;
        false ->
            false
    end.

create_message(Cmd, Enc, Acc, Data) ->
    Length = integer_to_list(string:len(Data)),
    Msg =   "Client-Command: " ++ Cmd ++ "\n" ++
            "Client-Encoding: " ++ Enc ++ "\n" ++
            "Content-Size: " ++ Length ++ "\n" ++
            "GGS-Version: 1.0\n" ++
            "Accept: " ++ Acc ++ "\n" ++
            "\n" ++
            Data,
    Msg.

%% Internal helpers
do_parse(Data, ParsedMessage) ->
    NewLinePos = string:chr(Data, $\n),
    Line = string:substr(Data, 1, NewLinePos-1),
    Tokens = re:split(Line, ": ", [{return, list}]),
    case handle(Tokens) of
        {Command, more} ->
            do_parse(string:substr(Data, NewLinePos+1), ParsedMessage ++ [Command]);
        {separator, data_next} ->
            {_, Value} = lists:keyfind(content_len, 1, ParsedMessage),
            {ContentLength, []} = string:to_integer(Value),
            {data, ArgumentData} = handle_data(string:substr(Data, NewLinePos+1), ContentLength),
            {ParsedMessage, ArgumentData}
    end.

handle([[]]) ->
    {separator, data_next};
handle(["Server-Command", Param]) ->
    {{srv_cmd, Param}, more};
handle(["Game-Command", Param]) ->
    {{game_cmd, Param}, more};
handle(["Content-Length", Param]) ->
    {{content_len, Param}, more};
handle(["Token", Param]) ->
    {{token, Param}, more};
handle(["Content-Type", Param]) ->
    {{content_type, Param}, more}.

handle_data(Data, Length) ->
    {data, string:substr(Data,1,Length)}.


prettify({Args, Data}) ->
    case lists:keyfind(srv_cmd, 1, Args) of
        {_, Value} ->
            {srv_cmd, Value, Args, Data};
        _Other ->
            case lists:keyfind(game_cmd, 1, Args) of
                {_, Value} ->
                    {game_cmd, Value, Args, Data};
                _ ->
                    ok
            end
    end.

