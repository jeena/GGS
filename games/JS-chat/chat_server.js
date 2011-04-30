function playerCommand(player_id, command, args) {
    if(command == "/nick") {
        changeNick(player_id, args);
    } else if(command == "message") {
        message(player_id, args);
    }
}

function changeNick(player_id, nick) {
    var old_nick = GGS.localStorage.getItem("nick_" + player_id);
    GGS.localStorage.setItem("nick_" + player_id, nick);
    
    if (!old_nick) {
        GGS.sendCommandToAll("notice", nick + " joined");
    } else {
        GGS.sendCommandToAll("notice", old_nick + " is now called " + nick);
    }
}

function message(player_id, message) {
    var nick = GGS.localStorage.getItem("nick_" + player_id);
    GGS.sendCommandToAll(player_id, 'message', nick + "> " + message);  
}