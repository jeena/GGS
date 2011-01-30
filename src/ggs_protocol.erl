%% This is a parser for JSON implemented using mochijson2
%% Don't feed it anything other than valid JSON.

-module(ggs_protocol).
-export([parse/1]).

parse(JSONData) ->
    {struct, Struct} = js_mochijson2:decode(JSONData),
    io:format("~p~n", [Struct]),
    [{RequestType, Rest}] = Struct,%proplists:get_value(<<"request">>, Struct),
    case RequestType of
        <<"define">> ->
            %Name = proplists:get_value(<<"name">>, Rest),
            %{struct, Name} = proplists:get_value(<<"name">>, Rest),
            {struct, Rest2} = Rest,
            io:format("~p", [Rest2]),
            ok_you_said_define;
        <<"call">> ->
            ok_you_said_call;
        Other ->
            io:format("~p", [RequestType]),
            ok_i_dont_understand
    end.
