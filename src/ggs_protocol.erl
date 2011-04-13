%% Parse a string formatted with the GGS protocol using
%% an FSM. Each char is put into the FSM, which incrementally
%% builds a list of strings which represent the complete
%% message.

-module(ggs_protocol).
-export([parse/2, getToken/1, create_message/4, create_message/1,
        expect_headers/2, expect_data_section/2,
        expect_headers/3, expect_data_section/3]).

%% tests
-export([to_dictionary/2]).

% gen_fsm callbacks..
-export([init/1, handle_info/2, terminate/2, code_change/3, start_link/0]).


%% API Functions
parse(Protocol, Data) ->
    lists:foreach(fun(X) -> gen_fsm:sync_send_event(Protocol, {char, X}) end, Data).


start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
    
% Start state: {[""],0}, meaning:
%   - Start with no strings parsed
%   - Start with a data-section-lengths of 0
init([]) ->
    {ok, expect_headers, {[""], 0}}.

getToken(Parsed) ->
    case lists:keyfind(token, 1, Parsed) of
        {_, Value} ->
            Value;
        false ->
            false
    end.

create_message({Command, Data}) ->
    create_message(Command, "text", "text", Data).

%% Assemble a message which can b
%e used as a reply to a client
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

%%% Transitions
expect_headers(_Event, State) ->
    {next_state, expect_headers, State}.
expect_data_section(_Event, State) ->
    {next_state, expect_data_section, State}.


%%% End transitions
expect_headers({char, $\n}, {Pid,_}, {Strings, Remains}) ->
    [LastMessage|_] = Strings,
    case LastMessage of
        "" -> % We have a data section.. Last line should thus be the content length.
              % FIXME: the Content-Length doesn't have to be the last Header line
            [LastMessage, SecondLast | Rest] = Strings,
            case re:split(SecondLast, ": ", [{return, list}]) of
                ["Content-Length", X] ->
                    {Int,_} = string:to_integer(X),
                    case Int of
                        0 -> ggs_player:notify_game(Pid, prettify(to_dictionary([SecondLast|Rest], []), [])),
                             {reply, ok, expect_headers, {[""], 0}};
                        _ -> {reply, ok, expect_data_section, {[""|Strings], Int}}
                    end;
                _Other -> ok
            end;
        _Other ->
            {reply,ok,expect_headers, {[""|Strings], Remains}}
    end;

expect_headers({char, Char}, _From, {[Current|Rest], Remains}) ->
    NewCurrent = Current ++ [Char],
    {reply, ok, expect_headers, {[NewCurrent|Rest], Remains}}.


expect_data_section({char, Char}, From, {Strings, Remains}) ->
    case Remains of
        0 ->
            [LastMsg,_|Rest] = Strings,
            {Pid,_} = From,
            ggs_player:notify_game(Pid, prettify(to_dictionary(Rest, []), LastMsg)),
            {reply, ok, expect_headers, {[[Char]], 0}};
        _Other ->    [LastMsg|Rest] = Strings,
                    NewMsg = LastMsg ++ [Char],
                    {reply, ok, expect_data_section, {[NewMsg|Rest], Remains-1}}
    end.
    
%handle_call(_Msg, _From, State) ->
%    {noreply, State}.
handle_info(_Msg, State) -> 
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


prettify(Args, Data) ->
    case lists:keyfind("Server-Command", 1, Args) of
        {_, Value} ->
            {srv_cmd, Value, Args, Data};
        _Other ->
            case lists:keyfind("Game-Command", 1, Args) of
                {_, Value} ->
                    {game_cmd, Value, Args, Data};
                Other ->
                    ok
            end
    end.

to_dictionary([], Dict) ->
    Dict;
to_dictionary([S|Strings], Dict) ->
    [First, Snd] = re:split(S, ": ", [{return, list}]),
    to_dictionary(Strings, [{First, Snd}|Dict]).
    
