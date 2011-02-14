-module(ggs_protocol).
-export([parse/1]).

parse(Data) ->
    Message =string:tokens(Data, "\n"),
    % Turn "A: B" pairs into "{A, B}" tuples, for searching.
    MsgKV = lists:map((fun(Str) -> 
                    list_to_tuple(string:tokens(Str, ": ")) end
              ), Message),
    % Hacky way to build a tuple, filter out not_found later on
    Processed = { 
                case lists:keysearch("Command", 1, MsgKV) of
                    {value,{_, "define"}} ->
                        define;
                    {value,{_, "call"}} ->
                        call;
                    {value,{_, "hello"}} ->
                        hello;
                    false ->
                        not_found
                end,
                case lists:keysearch("Token", 1, MsgKV) of
                    {value,{_, Value}} ->
                        Value;
                    false ->
                        not_found
                end,
                case lists:keysearch("Content-Length", 1, MsgKV) of
                    {value,{_, Value}} ->
                        {Length, _} = string:to_integer(Value),
                        [_|Cont] = re:split(Data, "\n\n",[{return,list}]),
                        Content = string:join(Cont, "\n\n"),
                        Payload = string:substr(Content,1,Length),
                        Payload;
                    false ->
                        not_found
                end
                },
    gen_server:cast(ggs_server, Processed).
