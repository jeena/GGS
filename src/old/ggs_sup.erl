-module(ggs_sup).
-behaviour(supervisor).

%% API
-export([start/1, start_link/1]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

start(Port) ->
    [FirstArg] = Port, 
    {IntPort, _} = string:to_integer(FirstArg),
    start_link(IntPort).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

init([Port]) ->
    Server =    {ggs_server_sup, 
                    {ggs_server_sup, start_link, [Port]},
                    permanent, 
                    2000, 
                    worker, 
                    [ggs_server_sup]
                },
    Children = [Server],

    RestartStrategy = { one_for_one, % Restart only crashing child
                        10,          % Allow ten crashes per..
                        1            % 1 second, then crash supervisor.
                      },
    {ok, {RestartStrategy, Children}}.

