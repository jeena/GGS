function TicTacToe() {};
TicTacToe.prototype.init = function() {

}

TicTacToe.prototype.userAllowed = function(user) {
	if(GGS.users.length <= 2) return true
	else return false;
}

TicTacToe.prototype.userAdded = function(user) {
	GGS.localStorage.setItem("p" + GGS.users.length + "_id", user.id);
	
	if (GGS.users.length == 2) {
		GGS.world.setItem("game_board", JSON.stringify(this.newGameBoard(3)));
		GGS.localStorage.setItem("next_player", 1);
		this.getUser(GGS.localStorage.getItem("p1_id")).sendCommand("yourturn");
	}
}

TicTacToe.prototype.getUser = function(user_id) {
	for (var i=0; i < GGS.users.length; i++) {
		if(GGS.users[i].id == user_id) {
			return GGS.users[i];
		}
	}
}

TicTacToe.prototype.newGameBoard = function(rows) {
	// Initiate game with empty rows and columns
	var gameBoard = [];
	for (var i=0; i < rows; i++) {
		gameBoard[i] = [];
		for (var j=0; j < rows; j++) {
			gameBoard[i][j] = 0;
		}
	}
	return gameBoard;
}

TicTacToe.prototype.userCommand = function(user, command, args) {
	var nextPlayer = GGS.localStorage.getItem("next_player");
	var p1_id = GGS.localStorage.getItem("p1_id");
	var p2_id = GGS.localStorage.getItem("p2_id");
	var valid = false;
	
	if(nextPlayer == 1 && user.id == p1_id) {
		valid = true;
	} else if (nextPlayer == 2 && user.id == p2_id) {
		valid = true;
	}
	
	if (valid) {
		if (command == "set") {
			
			var p = nextPlayer;
			var props = JSON.parse(args);
			var gameBoard = JSON.parse(GGS.world.getItem("game_board"));
			
			if (gameBoard[props.x][props.y] == 0) {
				gameBoard[props.x][props.y] = p;
				GGS.world.setItem("game_board", JSON.stringify(gameBoard))
				if (this.checkIfWon(p, gameBoard)) {
					if (p == 1) {
						this.getUser(p1_id).sendCommand("winner", "You win!");
						this.getUser(p2_id).sendCommand("loser", "You lose!");									
					} else {
						this.getUser(p1_id).sendCommand("loser", "You lose!");				
						this.getUser(p2_id).sendCommand("winner", "You win!");
					}
				}
				
				if (nextPlayer == 1) {
					GGS.localStorage.setItem("next_player", 2);
					this.getUser(p1_id).sendCommand("yourturn");
				} else {
					GGS.localStorage.setItem("next_player", 1);
					this.getUser(p2_id).sendCommand("yourturn");					
				}
				
			} else {
				user.sendCommand("warning", "Already set, chose something else.");
			}
		} else throw "TTTS: Unknown command " + command;		
	} else {
		user.sendCommand("warning", "Not your turn!");
	}
}

TicTacToe.prototype.checkIfWon = function(player, gameBoard) {

	var rows = gameBoard.length;

	for (i = 0; i < rows; ++i) {
		for (j = 0; j < rows; ++j) {
			if (gameBoard[i][j] != player) {
				break;
			}
		}
		if (j == rows) {
			return true;
		}

		for (j = 0; j < rows; ++j) {
			if (gameBoard[j][i] != player) {
				break;
			}
		}
		if (j == rows) {
			return true;			
		}
	}
	
	// Now check diagnols
	for (i = 0; i < rows; ++i) {
		if (gameBoard[i][i] != player) {
			break;			
		}
	}
	
	if (i == rows) {
		return true;		
	}
	
	for (i = 0; i < rows; ++i) {
		if (gameBoard[i][rows - i - 1] != player) {
			break;			
		}
	}
	if (i == rows) {
		return true;
	}
	
	return false;
}

function main() {
	return new TicTacToe();
}