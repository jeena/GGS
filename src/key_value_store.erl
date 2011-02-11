-module(key_value_store).
-export(start/0,stop/0,set_item/2,get_item/1,get/1,length(0,clean/0).

start() ->
	spawn_link(fun() -> loop([]) end).
	
stop() ->
	self() ! {stop}
	
loop(Touples) ->
	receive ->
		{set_item, Key, Value} ->
			Touple = find(Touples, Key)
		{stop} ->
			{'EXIT', normal}
			
find([], _) -> false;
find([{Key, _}|Tuples], Match) ->
    case Key == Match of
        true -> true;
        false -> find(Tuples, Match)
    end.