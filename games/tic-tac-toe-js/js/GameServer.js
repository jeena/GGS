// This is just a simulator of a game server to find
// out which methods we need. This will be rewritten
// in erlang.

// Game main() function should return a instance of the game
// Game Methods:
// userAllowed() op
// userAdded() op
// userCommand() m

function GameServer() {
	this.init();
}
GameServer.prototype.init = function() {
	this.games = {};
}

GameServer.prototype.addGame = function(game_name, instance) {
	if (typeof game_name == "string" && game_name != "" && typeof instance == "object") {
		var aGame = {
			instance: instance,
			users: [],
			world: {},
			localStorage: {}
		}
		this.games[game_name] = aGame;
		this.games[game_name].users
	} else throw "GGS: Could not add game " + game_name;
}

GameServer.prototype.addClient = function(game_name, client) {
	var game = this.games[game_name];
	if (typeof game != "undefined") {
		if (typeof game.instance.userAllowed == "undefined" || typeof game.instance.userAllowed == "function" && game.instance.userAllowed(user)) {
			var user = new User(game.users.length, client);
			game.users.push(user);
			if (typeof game.instance.userAdded == "function") {
				game.instance.userAdded(user);
			}
		} else throw "GGS: Unable to add client to game " + game_name;
	} else throw "GGS: No such game " + game_name;
}

GameServer.prototype.users = function(game_name) {
	var game = this.games[game_name];
	if (game != "undefined") {
		return game.users;
	} else throw "GGS: Unknown game " + game_name;
}

// Client calls this command, just a convinience method in the simulator
GameServer.prototype.callCommand = function(game_name, client, command, attrs) {
	var game = this.games[game_name];
	if (typeof game != "undefined") {
		if(typeof game.instance.userCommand == "function") {
			var user = null;
			for (var i=0; i < game.users.length; i++) {
				if (game.users[i].isClient(client)) {
					user = game.users[i];
				}
			}
			if (user != null) {
				game.instance.userCommand(user, command, attrs);				
			} else throw "GGS: User not allowed";
		} else throw "GGS: No such method userCommand() in game " + game_name;
	} else throw "GGS: No such game " + game_name;
}

GameServer.prototype.set = function(game_name, type, key, value) {
	var game = this.games[game_name];
	if (game != "undefined") {
		if (type == "world" || type == "localStorage") {
			if (typeof key == "string" && (typeof value == "string" || typeof value == "number")) {
				game[type][key] = value;	
				if (type == "world") {
					var args = {};
					args[key] = JSON.parse(value);
					for (var i=0; i < game.users.length; i++) {
						game.users[i].sendCommand("world_set", JSON.stringify(args));
					}
				}
			} else throw "GGS: Key has to be a string and value string or number";
		} else throw "GGS: Unknown type " + type;
	} else throw "GGS: No such game" + game_name;
}

GameServer.prototype.get = function(game_name, type, key) {
	var game = this.games[game_name];
	if (game != "undefined") {
		return game[type][key];
	} else throw "GGS: No such game" + game_name;
}

GameServer.prototype.key = function(game_name, type, position) {
	var game = this.games[game_name];
	if (game != "undefined") {
		var i = 0;
		for (var key in game[type]) {
			if(i++ == position) {
				return key;
			}
		}
		return null;
	} else throw "GGS: No such game" + game_name;
}

GameServer.prototype.length = function(game_name, type) {
	var game = this.games[game_name];
	if (game != "undefined") {
		var l = 0;
		for (var i in game[type]) {
			l++;
		}
		return l;
	} else throw "GGS: No such game" + game_name;
}

GameServer.prototype.remove = function(game_name, type) {
	var game = this.games[game_name];
	if (game != "undefined") {
		delete game[key];
	} else throw "GGS: Unknown game " + game_name
}

GameServer.prototype.clear = function(game_name, type) {
	var game = this.games[game_name];
	if (game != "undefined") {
		game = {};
	} else throw "GGS: Unknown game " + game_name		
}

var GameServer = new GameServer();




// User object
// to talk to the client it should have implemented 
// the method commandCalled
function User(id, client) {
	var id = id;
	var client = client;
	
	return {
		id: id,
		sendCommand: function(command, args) {
			client.commandCalled(command, args);
		},
		isClient: function(c) {
			return c == client;
		}
	}
}


function Storage(game_name, type) {
	if (type == "world" || type == "localStorage") {
		this.type = type;
		this.gameName = game_name;
		var self = this;
		
		return {
			setItem: function(key, value) {
				GameServer.set(self.gameName, self.type, key, value);
			},
			getItem: function(key) {
				return GameServer.get(self.gameName, self.type, key);
			},
			key: function(position) {
				return GameServer.key(self.gameName, self.type, position);
			},
			length: {
				get: function() {
						return GameServer.length(self.gameName, self.type);					
				}
			},
			removeItem: function(key) {
				GameServer.remove(self.gameName, self.type, key);
			},
			clear: function() {
				GameServer.clear(self.gameName, self.type);
			}
		}
	} else throw "GGS: No such storage available " + type;
}
