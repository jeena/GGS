function TicTacToeServer(rows) {}

TicTacToeServer.prototype.init = function() {

}

TicTacToeServer.newGame = function() {
	this.rows = 3;
	
	// Initiate game with empty rows and columns
	this.gameBoard = [];
	for (var i=0; i < this.rows; i++) {
		this.gameBoard[i] = [];
		for (var j=0; i < this.rows; i++) {
			this.gameBoard[i][j] = '';
		}
	}
}

TicTacToeServer.prototype.checkIfWon = function(player) {

	for (i = 0; i < this.rows; ++i) {
		for (j = 0; j < this.rows; ++j) {
			if (this.gameBoard[i][j] != 'X') {
				break;
			}
		}
		if (j == this.rows) {
			return true;
		}

		for (j = 0; j < this.rows; ++j) {
			if (this.gameBoard[j][i] != 'X') {
				break;
			}
		}
		if (j == this.rows) {
			return true;			
		}
	}
	
	// Now check diagnols
	for (i = 0; i < this.rows; ++i) {
		if (this.gameBoard[i][i] != 'X') {
			break;			
		}
	}
	
	if (i == this.rows) {
		return true;		
	}
	
	for (i = 0; i < this.rows; ++i) {
		if (this.gameBoard[i][this.rows - i - 1] != 'X') {
			break;			
		}
	}
	if (i == this.rows) {
		return true;
	}
	
	return false;
}