-module(ggs_protocol).
-export([parse/1, getToken/1]).

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
            gen_server:cast(ggs_server, {srv_cmd, Value, Args, Data});
        _Other ->
            case lists:keyfind(game_cmd, 1, Args) of
                {_, Value} ->
                    gen_server:cast(ggs_server, {game_cmd, Value, Args, Data});
                _ ->
                    ok
            end
    end.

