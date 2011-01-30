function GGS(game_name) {
	var world = new Storage(game_name, "world");
	this.__defineGetter__("world", function(){
		return world;
	});

	var localStorage = new Storage(game_name, "localStorage");
	this.__defineGetter__("localStorage", function(){
		return localStorage;
	});
	
	var game_n = game_name;
	this.__defineGetter__("users", function(){
		return GameServer.users(game_n);
	});
}
