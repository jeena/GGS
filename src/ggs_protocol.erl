%% This is a parser for JSON implemented using mochijson2
%% Don't feed it anything other than valid JSON.

-module(ggs_protocol).
-export([parse/1]).

parse(JSONData) ->
    {struct, Struct} = js_mochijson2:decode(JSONData),
    RequestType = proplists:get_value(<<"request">>, Struct),
    case RequestType of
        <<"define">> ->
            ok_you_said_define;
        Other ->
            io:format("~p", [RequestType]),
            ok_i_dont_understand
    end.
