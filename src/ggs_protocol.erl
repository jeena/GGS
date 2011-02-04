%% This is a parser for JSON implemented using mochijson2
%% Don't feed it anything other than valid JSON.

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
    Processed.
%    case Message of
%        ["__get_vms"]                           ->
%            {vms};
%        [RefID, "__error", Size, Message      ] ->
%            {ok, you_said_error};
%        [_,     "__boot",  _ ]                  ->
%            {ok, you_said_boot};
%        [RefID, "__stop",  _]                   ->
%            {ok, you_said_stop};
%        [RefID, "__start", _]                   ->
%            {ok, you_said_start};
%        ["__hello",        _]                   ->
%            {hello};
%        [RefID, "__define",_, JavaScript  ]     ->
%            {ok, you_said_define};
%        [RefID, "__echo", Length, Msg ]         ->
%            {Ref, _} = string:to_integer(RefID),
%            {echo, Ref, Length, Msg};
%        [RefID, Command,   _, Parameter      ]  ->
%            {cmd, Command, Parameter};
%        %% Debugging tools, not for production use
%        ["__crash"]                             ->
%            {crash, 0};
%        %% End debugging tools
%        Other ->
%            {out_of_bounds, Other}
%    end.
