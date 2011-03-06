function Storage(type) {
	if (type == "world" || type == "localStorage" || type == "players") {
		this.type = type;
		var self = this;

		return {
			setItem: function(key, value) {
				if(this.type != "players")
					callErlang("ggs_db setItem " + escapeErlang([GGS.tableToken, self.type, key, value]));
				else
					throw "No such method setItem()";
			},
			getItem: function(key) {
				return callErlang("ggs_db getItem " + escapeErlang([GGS.tableToken, self.type, key]));
			},
			key: function(position) {
				return callErlang("ggs_db key " + escapeErlang([GGS.tableToken, self.type, position]));
			},
			length: {
				get: function() {
					return callErlang("ggs_db length " + escapeErlang([GGS.tableToken, self.type]));
				}
			},
			removeItem: function(key) {
				if(this.type != "players")
					callErlang("ggs_db removeItem " + escapeErlang([GGS.tableToken, self.type, key]));
				else
					throw "No such method removeItem()";
			},
			clear: function() {
				if(this.type != "players")
					callErlang("ggs_db clear " + escapeErlang([GGS.tableToken, self.type]));
				else
					throw "No such method clear()";
			}
		}
	} else throw "GGS: No such storage available " + type;
}

function _GGS(tableToken) {
	
	this.tableToken = tableToken;
		
	var world = new Storage("world");
	this.__defineGetter__("world", function() {
		return world;
	});
	

	var localStorage = new Storage("localStorage");
	this.__defineGetter__("localStorage", function() {
		return localStorage;
	});
	
	var players = new Storage("players");
	this.__defineGetter__("players", function() {
		return players;
	});
	
	var tableToken = this.tableToken;
	this.__defineGetter__("tableToken", function() {
		return tableToken;
	});
	
}

_GGS.prototype.sendCommandToAll = function(command, args) {
	var message = "{" + command + "," + args + "}";
	callErlang("ggs_table send_command_to_all " + escapeErlang([this.tableToken, message]));
}

_GGS.prototype.serverLog = function(message) {
	callErlang("'error_logger info_msg " + escapeErlang([message]) + "'");
}

function escapeErlang(args) {
	var str = JSON.stringify(args);
	str = str.replace("'", "\\\'");
	return str;
}

function Player(token) {
	
	var playerToken = token;
	this.__defineGetter__("id", function() {
		return playerToken;
	});
	
	return {
		sendCommand: function(command, args) {
			ejsLog("/tmp/ggs-test.txt", "'ggs_table send_command " + escapeErlang([GGS.tableToken+ "", playerToken, command, args])+"'");
			//callErlang("'ggs_table send_command " + escapeErlang([GGS.tableToken+ "", playerToken, command, args]) + "'");
			ejsLog("/tmp/ggs-test.txt", "done");
		}
	}
}



/*
// ------------ Player stuff -------------
// TODO: remove this later on

function playerCommand(player, command, args) {
	switch(command) {
		case "greet":
			player.sendCommand("notification", "Welcome on our server!");
			var new_nick = args;
			if(validNick(new_nick)) {
				newNick(new_nick);
				GGS.sendCommandToAll("joined", new_nick);				
			}
			break;
		case "chat":
			GGS.sendCommandToAll("chat", args);
			break;
		case "uname":
			player.sendCommand("notice", callErlang("os cmd [\"uname -a\"]"))
			break;
		case "lplayers":
			listUsers(player);
			break;
		case "nick":
			if(validNick(new_nick)) {
				newNick(new_nick);
				GGS.sendCommandToAll("nickchange", old_nick + "," + nicks[player.id]);				
			}
			break;
		default:
			player.sendCommand("error", "Command not found");
			break;
	}
}

function validNick(new_nick) {
	if(new_nick.lastIndexOf(",") != -1) {
		player.sendCommand("error", "Mallformed nick " + new_nick);
		return false;
	}

	var nicks_s = GGS.localStorage("nicks");
	var nicks = {};
	if(nicks_s != "") { // if not the first player
		nicks = JSON.parse(nicks_s);
	}
	for (var id in nicks) {
		if (nicks[id] == new_nick) {
			player.sendCommand("error", "Nick " + new_nick + " is already taken");
			return false;
		}
	}
	
	return true;
}


function newNick(new_nick) {
	var nicks_s = GGS.localStorage("nicks");
	var nicks = {};
	if(nicks_s != "") { // if not the first player
		nicks = JSON.parse(nicks_s);
	}
	
	nicks[player.id] = new_nick;
	old_nick = nicks[player.id];
	GGS.localStorage.setItem("nicks", JSON.stringify(nicks));	
}

function listUsers(player) {
	var nicks = JSON.parse(GGS.localStorage.getItem("nicks"));
	var nicks_a = [];
	for(var id in nicks) {
		nicks_a.push(nicks[id])
	}
	player.sendCommand("nicklist", nicks_a.join(","));
}
*/