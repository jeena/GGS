// This first version doesn't take networking into account yet.

function TicTacToe(container_id) {			
	this.player_turn = 1;
	this.rows = 3;

	this.container = document.getElementById(container_id);
	var table = document.createElement("table");
	var tr = document.createElement("tr");
	var td = document.createElement("td");
	
	for(var i=0; i < this.rows; i++) {
		var atr = tr.cloneNode();
		
		for(var j=0; j < this.rows; j++) {
			
			
			var atd = td.cloneNode();
			var self = this;
			atd.onclick = function(e) {
				if (e.target.innerHTML == "") {
					if (self.player_turn == 1) {
						e.target.innerHTML = "X";
						self.player_turn = 2;
					} else {
						e.target.innerHTML = "O";
						self.player_turn = 1;							
					}							
				}
			}
			atr.appendChild(atd);
		}
		table.appendChild(atr);
	}
	
	this.container.innerHTML = "";
	this.container.appendChild(table)
}
