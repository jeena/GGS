-module(pong_bot).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([ggsNetworkReceivedCommandWithArgs/2,set_game_token/1,get_game_token/0]).

start_link() ->
    gen_server:start_link({global, pong_bot}, pong_bot, [], []).


init(_Args) ->
    Player1 = new_pos(),
    Player2 = new_pos(),
    Ball = new_pos(),
    Paused = true,
    SendStart = false,
    GGSNetwork = ggs_network:connect(), %Localhost is set internally inside 
                                        %ggs_network.
    State1 = dict:new(),
    State2 = dict:store(player1, Player1, State1),
    State3 = dict:store(player2, Player2, State2),
    State4 = dict:store(ball, Ball, State3),
    State5 = dict:store(paused, Paused, State4),
    State6 = dict:store(send_start, SendStart, State5), 
    State = dict:store(ggs_network, GGSNetwork, State6),
    State.

new_pos() ->
    {0, 0}.


ggsNetworkReceivedCommandWithArgs(Command, Args) ->
    case Command of
        "welcome" -> 
            welcome(Args);
        "ball" -> 
            ball(Args);
        "player1_y" ->
            player1_y(Args);
        "player2_y" ->
            player2_y(Args);
        "game" ->
            game(Args);
        "player1_points" ->
            new_round();
        "player2_points" ->
            new_round()
    end.   

welcome(Who_am_I) ->
    case Who_am_I of 
        1 ->
            Me = gen_server:call(pong_bot, player1),
            gen_server:cast(pong_bot, {me, Me});
        2 ->
            Me = gen_server:call(pong_bot, player2),
            gen_server:cast(pong_bot, {me, Me})
    end,
    
    loop().
    
loop() ->
    timer:sleep(300),
    gameTick(),
    loop().
    
gameTick() ->
    GamePaused = gen_server:call(pong_bot, paused),       
    SendStart = gen_server:call(pong_bot, send_start),
    
    case GamePaused of
        true ->
            case SendStart of
                false ->
                    ggs_network:send_command("start"),
                    gen_server:cast(pong_bot, {send_start, true})
            end;
        false ->
            Ball = gen_server:call(pong_bot, ball),
            {_, BallY} = Ball,
            Me = gen_server:call(pong_bot, me),
            {_, MeY} = Me,
            
            case BallY < (MeY - 5) of
                true ->
                    ggs_network:send_command("up");
                false ->
                    ggs_network:send_command("down")
            end
    end.
            
            
            
             
ball(Pos_s) ->
    PosList = string:tokens(Pos_s, ","),
    XStr = lists:nth(1,PosList),
    YStr = lists:nth(1,PosList),
    X = string:to_integer(XStr),
    Y = string:to_integer(YStr),
    Pos = {X, Y},
    gen_server:cast(pong_bot, {ball, Pos}).

player1_y(YStr) ->
    Y = string:to_integer(YStr),
    gen_server:cast(pong_bot, {player1_y, Y}).

player2_y(YStr) ->
    Y = string:to_integer(YStr),
    gen_server:cast(pong_bot, {player2_y, Y}).

game(WaitOrStart) ->
    case WaitOrStart of
        "wait" ->
            ok;
        _ ->
            gen_server:cast(pong_bot, {paused, false})
    end.


new_round() ->
    Paused = true,
    SendStart = false,    
    gen_server:cast(pong_bot, {new_round, Paused, SendStart}). 
    
    
set_game_token(GameToken) ->
    gen_server:cast({global, pong_bot}, {game_token, GameToken}).  

get_game_token() ->
    gen_server:call({global, pong_bot}, game_token).
    
handle_call(player1, _From, State) ->
    Player1 = dict:fetch(player1, State),
    {reply, Player1, State};        

handle_call(player1_y, _From, State) ->
    {_,Y} = dict:fetch(player1, State),
    {reply, Y, State};
    
handle_call(player2_y, _From, State) ->
    {_,Y} = dict:fetch(player2, State),
    {reply, Y, State};    
    
handle_call(game_token, _From, State) ->
    io:format("Handle call game_token~n"),
    GameToken = dict:fetch(game_token, State),
    {reply, GameToken, State}.    

handle_cast({me, Me}, State) ->
    NewState = dict:store(me, Me, State),
    {noreply, NewState};
    
handle_cast({ball, Pos}, State) ->
    NewState = dict:store(ball, Pos, State),
    {noreply, NewState};
    
handle_cast({player1_y, Y}, State) ->
    {OldX, _} = dict:fetch(player1, State),
    NewPlayer1 = {OldX, Y},
    NewState = dict:store(player1, NewPlayer1, State),
    {noreply, NewState};
    
handle_cast({player2_y, Y}, State) ->
    {OldX, _} = dict:fetch(player2, State),
    NewPlayer2 = {OldX, Y},
    NewState = dict:store(player2, NewPlayer2, State),
    {noreply, NewState};

handle_cast({paused, Paused}, State) ->
    NewState = dict:store(paused, Paused, State),
    {noreply, NewState};
    
handle_cast({new_rouned, Paused, SendStart}, State) ->
    State1 = dict:store(paused, Paused, State),
    NewState = dict:store(send_start, SendStart, State1),
    {noreply, NewState};
    
handle_cast({game_token, Token}, State) ->
    io:format("Handle cast game_token~n"),
    NewState = dict:store(game_token, Token, State),
    {noreply, NewState}.
