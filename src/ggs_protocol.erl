%% This is a parser for JSON implemented using mochijson2
%% Don't feed it anything other than valid JSON.

-module(ggs_protocol).
-export([parse/1]).

parse(Data) ->
    Message = string:tokens(Data, " "),
    case Message of
        [RefID, "__error", Message      ] ->
            {ok, you_said_error};
        [_,     "__boot" ]                ->
            {ok, you_said_boot};
        [RefID, "__stop"]                 ->
            {ok, you_said_stop};
        [RefID, "__start"]                ->
            {ok, you_said_start};
        [_,     "__hello" , _           ] ->
            {ok, you_said_hello};
        [RefID, "__define", JavaScript  ] ->
            {ok, you_said_define};
        [RefID, Command, Parameter      ] ->
            {cmd, Command, Parameter}; 
        Other ->
            {out_of_bounds, Other}
    end.
