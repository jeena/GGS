%% This is a parser for JSON implemented using mochijson2
%% Don't feed it anything other than valid JSON.

-module(ggs_protocol).
-export([parse/1]).

parse(Data) ->
    Message = string:tokens(Data, " "),
    io:format(Message),
    case Message of
        [RefID, "__error", Size, Message      ] ->
            {ok, you_said_error};
        [_,     "__boot",  _ ]                  ->
            {ok, you_said_boot};
        [RefID, "__stop",  _]                   ->
            {ok, you_said_stop};
        [RefID, "__start", _]                   ->
            {ok, you_said_start};
        ["__hello",        _]                   ->
            {hello};
        [RefID, "__define",_, JavaScript  ]     ->
            {ok, you_said_define};
        [RefID, "__echo", Length, Msg ]         ->
            {Ref, _} = string:to_integer(RefID),
            {echo, Ref, Length, Msg};
        ["__crash"]                             ->
            {crash, 0};
        [RefID, Command,   _, Parameter      ]  ->
            {cmd, Command, Parameter}; 
        Other ->
            {out_of_bounds, Other}
    end.
