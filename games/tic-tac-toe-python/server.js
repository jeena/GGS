function playerCommand(player_id, command, args) {
    if (command == "hi") {
        hi(player_id);
    } else if (command == "set") {
        move(player_id, args);
    } else if (command == "new") {
        newGame();
    }
}

var ROWS = 3;

function hi(player_id) {
    var p1_id = GGS.localStorage.getItem("p1_id");
    var p2_id = GGS.localStorage.getItem("p2_id");
    if (!p1_id) {
        GGS.localStorage.setItem("p1_id", player_id);
        GGS.sendCommand(player_id, "welcome", "1");
    } else if (!p2_id) {
        GGS.localStorage.setItem("p2_id", player_id);
        GGS.sendCommand(player_id, "welcome", "2");
        newGame();
    } else {
        GGS.sendCommand(player_id, "not_welcome", "Already have 2 players on this table");
    }
}

function move(player_id, args) {
    var nextPlayer = GGS.localStorage.getItem("next_player");
	var p1_id = GGS.localStorage.getItem("p1_id");
	var p2_id = GGS.localStorage.getItem("p2_id");
	var valid = false;
	
	if(nextPlayer == 1 && player_id == p1_id) {
		valid = true;
	} else if (nextPlayer == 2 && player_id == p2_id) {
		valid = true;
	}
	
	if (valid) {
		var p = nextPlayer;
		var props = JSON.parse(args);
		var gameBoard = JSON.parse(GGS.localStorage.getItem("game_board"));
		
		if (gameBoard[props.x][props.y] == 0) {
			
			gameBoard[props.x][props.y] = p;
			GGS.localStorage.setItem("game_board", JSON.stringify(gameBoard));
			GGS.sendCommandToAll("game_board", boardAsString(gameBoard));
			GGS.log(this.checkIfWon(gameBoard))
			if (this.checkIfWon(gameBoard)) {
				if (p == 1) {
					GGS.sendCommand(p1_id, "winner", "You win!");
					GGS.sendCommand(p2_id, "loser", "You lose!");									
				} else {
					GGS.sendCommand(p1_id, "loser", "You lose!");				
					GGS.sendCommand(p2_id, "winner", "You win!");
				}
			}
			
			if (nextPlayer == 1) {
				GGS.localStorage.setItem("next_player", 2);
				GGS.sendCommand(p1_id, "yourturn", "");
			} else {
				GGS.localStorage.setItem("next_player", 1);
				GGS.sendCommand(p2_id, "yourturn", "");
			}
		} else {
			GGS.sendCommand(player_id, "warning", "Already set, chose something else.");
		}

	} else {
		GGS.sendCommand(player_id, "warning", "Not your turn!");
	}
}

function checkIfWon(gameBoard) {
    
	//var gameBoard = JSON.parse(GGS.localStorage.getItem("game_board"));

	for (i = 0; i < ROWS; ++i) {
		for (j = 0; j < ROWS; ++j) {
			if (gameBoard[i][j] != 1) {
				break;
			}
		}
		if (j == ROWS) {
			return true;
		}

		for (j = 0; j < ROWS; ++j) {
			if (gameBoard[j][i] != 1) {
				break;
			}
		}
		if (j == ROWS) {
			return true;			
		}
	}
	
	// Now check diagnols
	for (i = 0; i < ROWS; ++i) {
		if (gameBoard[i][i] != 1) {
			break;			
		}
	}
	
	if (i == ROWS) {
		return true;		
	}
	
	for (i = 0; i < ROWS; ++i) {
		if (gameBoard[i][ROWS - i - 1] != 1) {
			break;			
		}
	}
	if (i == ROWS) {
		return true;
	}
	
	return false;
}

function newGame() {
	// Initiate game with empty rows and columns
	var gameBoard = [];
	for (var i=0; i < ROWS; i++) {
		gameBoard[i] = [""];
		for (var j=0; j < ROWS; j++) {
			gameBoard[i][j] = 0;
		}
	}
	
	GGS.localStorage.setItem("game_board", JSON.stringify(gameBoard));
    	GGS.sendCommandToAll("new_game", "");
	GGS.sendCommandToAll("game_board", boardAsString(gameBoard));
	GGS.localStorage.setItem("next_player", 1);
}

function boardAsString(gameBoard) {
    var out = "";
    for (var i=0; i < ROWS; i++) {
        for (var j=0; j < ROWS; j++) {
            var p = gameBoard[i][j];
            if (p == 1) {
                out += "X";
            } else if (p == 2) {
                out +="O";
            } else {
                out += " ";
            }
        }
    }
    
    return out;
}