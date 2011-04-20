-module(pong_bot).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([ggsNetworkReceivedCommandWithArgs/2,set_game_token/1,get_game_token/0]).
-export([view/0, peek_socket/0]).


start_link() ->
    gen_server:start_link({global, pong_bot}, pong_bot, [], []),
    Socket = peek_socket(),
    spawn(fun() -> communication_loop(Socket) end),
    spawn(fun() -> game_loop() end ).

communication_loop(Socket) ->
    ggs_network:read(Socket),
    communication_loop(Socket).
 
 
peek_socket() ->
    gen_server:call({global, pong_bot}, socket).
    

init(_Args) ->
    Player1 = new_pos(),
    Player2 = new_pos(),
    Ball = new_pos(),
    Paused = true,
    Start = false,
    Socket = ggs_network:connect(), %Localhost is set internally inside ggs_network.
    State1 = dict:new(),
    State2 = dict:store(player1, Player1, State1),
    State3 = dict:store(player2, Player2, State2),
    State4 = dict:store(ball, Ball, State3),
    State5 = dict:store(paused, Paused, State4),
    State6 = dict:store(start, Start, State5), 
    State = dict:store(socket, Socket, State6),
    {ok, State}.

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
            new_round();
        _ -> ok
    end.

welcome(Who_am_I) ->
    io:format("Welcome begin~n"),
    io:format("I am player: ~s~n", [Who_am_I]),
    case Who_am_I of 
        "1" -> 
            io:format("I made myself into player 1~n"),
            Me = gen_server:call({global, pong_bot}, player1),
            gen_server:cast({global, pong_bot}, {me, Me});
        "2" ->
            io:format("I made myself into player 2~n"),
            Me = gen_server:call({global, pong_bot}, player2),
            gen_server:cast({global, pong_bot}, {me, Me})
    end.
    
   
    
game_loop() ->
    timer:sleep(300),
    gameTick(),
    game_loop().
    
gameTick() ->
    GamePaused = gen_server:call({global, pong_bot}, paused),       
    SendStart = gen_server:call({global, pong_bot}, start),
    
    case GamePaused of
        true ->
            case SendStart of
                false ->
                    io:format("Command start sent~n"),
                    ggs_network:send_command("start", ""),
                    gen_server:cast({global, pong_bot}, {start, true});
                true ->
                    ok
            end;
        false ->
            Ball = gen_server:call({global, pong_bot}, ball),
            {_, BallY} = Ball,
            Me = gen_server:call({global, pong_bot}, me),
            {_, MeY} = Me,
            
            case BallY < (MeY - 5) of
                true ->
                    ggs_network:send_command("up", "");
                _ -> case BallY > ( MeY - 5) of
                        true ->
                            ggs_network:send_command("down", "");
                        _ -> ok
                    end
            end
    end.
            
            
            
             
ball(Pos_s) ->
    io:format("Ball~n"),
    PosList = string:tokens(Pos_s, ","),
    XStr = lists:nth(1,PosList),
    YStr = lists:nth(2,PosList),
    X = list_to_integer(XStr),
    Y = list_to_integer(YStr),
    io:format("X~B~n", [X]),
    io:format("Y~B~n", [Y]),
    Pos = {X, Y},
    gen_server:cast({global, pong_bot}, {ball, Pos}).

player1_y(YStr) ->
    Y = list_to_integer(YStr),
    io:format("Y in integer: ~B~n", [Y]),
    gen_server:cast({global, pong_bot}, {player1_y, Y}).

player2_y(YStr) ->
    Y = list_to_integer(YStr),
    io:format("Y in integer: ~B~n", [Y]),
    gen_server:cast({global, pong_bot}, {player2_y, Y}).

game(WaitOrStart) ->
    case WaitOrStart of
        "wait" ->
            ok;
        _ ->
            gen_server:cast({global, pong_bot}, {paused, false})
    end.


new_round() ->
    Paused = true,
    SendStart = false,    
    gen_server:cast({global, pong_bot}, {new_round, Paused, SendStart}). 
    
    
set_game_token(GameToken) ->
    gen_server:cast({global, pong_bot}, {game_token, GameToken}).  

get_game_token() ->
    gen_server:call({global, pong_bot}, game_token).
    
view() ->
    gen_server:call({global, pong_bot}, game_token).
        
handle_call(player1, _From, State) ->
    io:format("Player1 before~n"),
    Player1 = dict:fetch(player1, State),
    io:format("Player1 after~n"),
    {reply, Player1, State};        

handle_call(player2, _From, State) ->
    Player2 = dict:fetch(player2, State),
    {reply, Player2, State}; 

handle_call(player1_y, _From, State) ->
    {_,Y} = dict:fetch(player1, State),
    {reply, Y, State};
    
handle_call(player2_y, _From, State) ->
    {_,Y} = dict:fetch(player2, State),
    {reply, Y, State};
    
handle_call(ball, _From, State) ->
    Ball = dict:fetch(ball, State),
    {reply, Ball, State};        

handle_call(me, _From, State) ->
    Me = dict:fetch(me, State),
    {reply, Me, State}; 

handle_call(game_token, _From, State) ->
    GameToken = dict:fetch(game_token, State),
    {reply, GameToken, State};

handle_call(view, _From, State) ->
    io:format("View the state.~n"),
%    StateFromList = lists:nth(1, State)
    {reply, State, State};

handle_call(socket, _From, State) ->
    Socket = dict:fetch(socket, State),
    %Socket = lists:nth(1, SocketInList),
    {reply, Socket, State};
    
handle_call(paused, _From, State) ->
    Paused = dict:fetch(paused, State),
    {reply, Paused, State};
    
handle_call(start, _From, State) ->
    Start = dict:fetch(start, State),
    {reply, Start, State}.
    
handle_cast({game_token, GameToken}, State) ->
    NewState = dict:store(game_token, GameToken, State),
    {noreply, NewState};
    
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
    
handle_cast({new_round, Paused, SendStart}, State) ->
    State1 = dict:store(paused, Paused, State),
    NewState = dict:store(send_start, SendStart, State1),
    {noreply, NewState};
    
handle_cast({start, Start}, State) ->
    NewState = dict:store(start, Start, State),
    {noreply, NewState}.
