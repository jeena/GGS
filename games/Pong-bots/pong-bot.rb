#!/usr/bin/env ruby -wKU

$: << "."

require 'ggs-network.rb'
require 'ggs-delegate.rb'

class PongBot
  include GGSDelegate
  
  def initialize
    @me = nil
    @player1 = Pos.new
    @player2 = Pos.new
    @ball = Pos.new
    @game_paused = true
    @send_start = false

    @ggs_network = GGSNetwork.new(self)
    @ggs_network.connect("192.168.0.1")
    #@ggs_network.connect()
  end

  def ggsNetworkReady(ggs_network, ready)
    @ggs_network.sendCommand("ready")
  end
  
  def ggsNetworkDefined(ggs_network, defined)
    # do nothing
  end
  
  def ggsNetworkReceivedCommandWithArgs(ggs_network, command, args)
    case command
      when "welcome"          then welcome(args)
      when "ball"             then ball(args)
      when "player1_y"        then player1_y(args)
      when "player2_y"        then player2_y(args)
      when "game"             then game(args)
      when "player1_points"   then new_round()
      when "player2_points"   then new_round()
    end
  end
  
  protected
  
  def gameTick()
    if @game_paused
      unless @send_start
        @ggs_network.sendCommand("start")
        @send_start = true
      end
    else
      if @ball.y < @me.y - 5
        @ggs_network.sendCommand("up")
      elsif @ball.y > @me.y - 5
        @ggs_network.sendCommand("down")
      end
    end
  end
  
  def welcome(who_am_i)
    if who_am_i == 1
      @me = @player1
    else
      @me = @player2
    end
    
    Thread.new {
      loop do
        gameTick()
        sleep 0.3
      end
    }
  end
  
  def ball(pos_s)
    x, y = pos_s.split(",")
    @ball.x, @ball.y = x.to_i, y.to_i
  end
  
  def player1_y(y)
    @player1.y = y.to_i
  end
  
  def player2_y(y)
    @player2.y = y.to_i
  end
  
  def game(wait_or_start)
    if wait_or_start == "wait"
    else
      @game_paused = false
    end
  end
  
  def new_round
    @game_paused = true
    @send_start = false
  end
  
  
  class Pos
    attr_accessor :x, :y
    def initialize
      @x = 0
      @y = 0
    end
  end
  
end

if __FILE__ == $0
    PongBot.new
end
