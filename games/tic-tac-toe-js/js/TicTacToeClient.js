// This first version doesn't take networking into account yet.

function TicTacToeClient(container, server) {
	this.server = server;
	this.container = container;	
	this.init();
}


TicTacToeClient.prototype.init = function() {
	this.rows = 3;
	this.spots = [];
	this.myturn = false;

	var table = document.createElement("table");
	var tr = document.createElement("tr");
	var td = document.createElement("td");
	
	var k = 0;
	
	for(var i=0; i < this.rows; i++) {
		var atr = tr.cloneNode(true);
		
		for(var j=0; j < this.rows; j++) {

			var atd = td.cloneNode(true);
			atd.id = + k++;
			this.spots.push(atd);
			
			var self = this;
			atd.onclick = function(e) {
				self.move(e.target.id);
			}
			atr.appendChild(atd);
		}
		table.appendChild(atr);
	}
	
	this.container.innerHTML = "";
	this.container.appendChild(table)
	this.messages = document.createElement("ol");
	this.container.appendChild(this.messages);
}

TicTacToeClient.prototype.move = function(id) {
	var args = {
		x: parseInt(id / this.rows),
		y: parseInt(id) % this.rows
	}
	this.server.callCommand("tictactoe", this, "set", JSON.stringify(args));		
}

TicTacToeClient.prototype.commandCalled = function(command, args) {
	if (command == "world_set") {
		var pars = JSON.parse(args);
		if (pars["game_board"]) {
			this.updateBoard(pars["game_board"]);	
		}
	} else if (command == "yourturn") {
		this.myturn = true;
	} else if (command == "warning") {
		this.addMessage(args);
	} else if (command == "loser") {
		this.addMessage(args);		
	} else if (command == "winner") {
		this.addMessage(args);
	}	else throw "TTTC: Unnown command " + command;
}

TicTacToeClient.prototype.addMessage = function(args) {
	var message = document.createElement("li");
	message.innerHTML = args;
	this.messages.appendChild(message)	
}

TicTacToeClient.prototype.updateBoard = function(gameBoardData) {
	var k = 0;
	for(var i=0; i<gameBoardData.length; i++) {
		var row = gameBoardData[i];
		for (var j=0; j<row.length; j++) {
			
			var t = "";
			if (row[j] == 1) {
				t = "X";
			} if (row[j] == 2) {
				t = "O";
			}
			
			this.spots[k++].innerHTML = t;
		}
	}
}