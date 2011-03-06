-module(tick).
-export([start/0]).

start() ->
	spawn(fun() -> loop() end).
	
loop() ->
	receive
		tick ->
			erlang:display("tick!"),
			timer:send_after(500, tick),
			loop();
		'EXIT' ->
			exit(normal)
	end.
	
% 1> c(tick).
%  {ok,tick}
% 2> Pid = tick:start().
%  <0.38.0>
% % Nothing happens :-(

% When I send it myself then it responds
% 3> Pid ! tick.
%  tick
% 5> 
