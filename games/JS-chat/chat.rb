#!/usr/bin/env ruby -wKU

$: << "."

require 'ggs-network.rb'
require 'ggs-delegate.rb'

class Chat
  include GGSDelegate
  
  def initialize
    @ggs_network = GGSNetwork.new(self)
    @ggs_network.connect("localhost")
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
    @ggs_network.sendCommand("message", message)
  end
  
end

if __FILE__ == $0
    Chat.new
end
