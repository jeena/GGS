


%% @spec start_link(GameToken::string()) -> socket()
%% @doc Spawn a new player process.
start_link(Socket) -> end.


%% @spec notify(Player::Pid(), From::Pid(), 
%%              {Command::String(), Message::string()}) -> ok
%% @doc send a message to a player.
notify(Player, From, Message) -> end.


%% @spec get_token() -> string()
%% @doc Get the player token.
get_token() -> end.


%% @spec stop(Table::pid()) -> Reason::string()
%% @doc Properly terminates the process.
stop(Table) -> end.
