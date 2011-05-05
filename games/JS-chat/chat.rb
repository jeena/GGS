#!/usr/bin/env ruby -wKU

$: << "."

require 'ggs-network.rb'
require 'ggs-delegate.rb'

class Chat
  include GGSDelegate
  
  def initialize
    print "Table token (empty for new): "
    table_token = gets.chomp
    @ggs_network = GGSNetwork.new(self, table_token)
    @ggs_network.connect("home.jeena.net", 9000)
  end
  
  def ggsNetworkReady(ggs_network, am_i_host)
    unless am_i_host
      source_code = File.open("chat_server.js", "rb").read
      @ggs_network.define(source_code)
    else
      ggsNetworkDefined(ggs_network, true)
    end
  end
  
  def ggsNetworkDefined(ggs_network, defined)
    if defined
      print "Your nickname: "
      nick = gets.chomp    
      @ggs_network.sendCommand("/nick", nick)
      Thread.new {
        loop do
          input
        end
      }
    else
      source_code = File.open("chat_server.js", "rb").read
      @ggs_network.define(source_code)  
    end
  end
  
  def ggsNetworkReceivedCommandWithArgs(ggs_network, command, args)
    case command
      when "message"  then message(args)
      when "notice"   then notice(args)
      when "pong"     then pong(args)
    end
  end
  
  protected
  
  def message(message)
    puts message
  end
  
  def notice(notice)
    puts "<#{notice}>"
  end
  
  def input
    message = gets.chomp
    if message[0..5] == "/nick "
      @ggs_network.sendCommand("/nick", message[6,-1])
    elsif message == "/ping"
      ping()
    else
      @ggs_network.sendCommand("message", message)      
    end
  end
  
  def ping
    @start_ping = Time.now
    @ggs_network.sendCommand("ping", @ggs_network.player_token)
  end

  def pong(id)
    puts "<pong: " + (Time.now - @start_ping).to_s + ">"
  end
  
end

if __FILE__ == $0
    Chat.new
end
