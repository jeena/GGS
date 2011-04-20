-module(pong_bot).
-behaviour(gen_server).
-export([start/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([ggsNetworkReceivedCommandWithArgs/3,set_game_token/2,get_game_token/1]).
-export([view/1, peek_socket/1]).

start(0) ->
    ok;
start(N) ->
    start_link(),
    timer:sleep(50),
    start(N - 1).

start_link() ->
    Ref = make_ref(),
    gen_server:start_link({global, {pong_bot, Ref}}, pong_bot, [], []),
    Socket = peek_socket(Ref),
    spawn(fun() -> communication_loop(Socket, Ref) end),
    spawn(fun() -> game_loop(Ref) end ).

communication_loop(Socket, Ref) ->
    ggs_network:read(Socket, Ref),
    communication_loop(Socket, Ref).
 
 
peek_socket(Ref) ->
    gen_server:call({global, {pong_bot, Ref}}, socket).
    

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


ggsNetworkReceivedCommandWithArgs(Command, Args, Ref) ->
    case Command of
        "welcome" -> 
            welcome(Args, Ref);
        "ball" -> 
            ball(Args, Ref);
        "player1_y" ->
            player1_y(Args, Ref);
        "player2_y" ->
            player2_y(Args, Ref);
        "game" ->
            game(Args, Ref);
        "player1_points" ->
            new_round(Ref);
        "player2_points" ->
            new_round(Ref);
        _ -> ok
    end.

welcome(Who_am_I, Ref) ->
    case Who_am_I of 
        "1" -> 
            Me = gen_server:call({global, {pong_bot, Ref}}, player1),
            gen_server:cast({global, {pong_bot, Ref}}, {me, Me});
        "2" ->
            Me = gen_server:call({global, {pong_bot, Ref}}, player2),
            gen_server:cast({global, {pong_bot, Ref}}, {me, Me})
    end.
    
   
    
game_loop(Ref) ->
    timer:sleep(300),
    gameTick(Ref),
    game_loop(Ref).
    
gameTick(Ref) ->
    GamePaused = gen_server:call({global, {pong_bot, Ref}}, paused),       
    SendStart = gen_server:call({global, {pong_bot, Ref}}, start),
    
    case GamePaused of
        true ->
            case SendStart of
                false ->
                    ggs_network:send_command("start", "", Ref),
                    gen_server:cast({global, {pong_bot, Ref}}, {start, true});
                true ->
                    ok
            end;
        false ->
            Ball = gen_server:call({global, {pong_bot, Ref}}, ball),
            {_, BallY} = Ball,
            Me = gen_server:call({global, {pong_bot, Ref}}, me),
            {_, MeY} = Me,
            
            case BallY < (MeY - 5) of
                true ->
                    ggs_network:send_command("up", "", Ref);
                _ -> case BallY > ( MeY - 5) of
                        true ->
                            ggs_network:send_command("down", "", Ref);
                        _ -> ok
                    end
            end
    end.
            
             
ball(Pos_s, Ref) ->
    PosList = string:tokens(Pos_s, ","),
    XStr = lists:nth(1,PosList),
    YStr = lists:nth(2,PosList),
    X = list_to_integer(XStr),
    Y = list_to_integer(YStr),
    Pos = {X, Y},
    gen_server:cast({global, {pong_bot, Ref}}, {ball, Pos}).

player1_y(YStr, Ref) ->
    Y = list_to_integer(YStr),
    gen_server:cast({global, {pong_bot, Ref}}, {player1_y, Y}).

player2_y(YStr, Ref) ->
    Y = list_to_integer(YStr),
    gen_server:cast({global, {pong_bot, Ref}}, {player2_y, Y}).

game(WaitOrStart, Ref) ->
    case WaitOrStart of
        "wait" ->
            ok;
        _ ->
            gen_server:cast({global, {pong_bot, Ref}}, {paused, false})
    end.


new_round(Ref) ->
    Paused = true,
    Start = false,    
    gen_server:cast({global, {pong_bot, Ref}}, {new_round, Paused, Start}). 
    
    
set_game_token(GameToken, Ref) ->
    gen_server:cast({global, {pong_bot, Ref}}, {game_token, GameToken}).  

get_game_token(Ref) ->
    gen_server:call({global, {pong_bot, Ref}}, game_token).
    
view(Ref) ->
    gen_server:call({global, {pong_bot, Ref}}, game_token).
        
handle_call(player1, _From, State) ->
    Player1 = dict:fetch(player1, State),
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
    {reply, State, State};

handle_call(socket, _From, State) ->
    Socket = dict:fetch(socket, State),
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
    
handle_cast({new_round, Paused, Start}, State) ->
    State1 = dict:store(paused, Paused, State),
    NewState = dict:store(start, Start, State1),
    {noreply, NewState};
    
handle_cast({start, Start}, State) ->
    NewState = dict:store(start, Start, State),
    {noreply, NewState}.
