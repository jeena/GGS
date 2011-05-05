#!/usr/bin/env ruby -wKU

$: << "."

require 'ggs-network.rb'
require 'ggs-delegate.rb'
require 'my-random.rb'
require 'readline'

class Chat
  include GGSDelegate
  include MyRandom
  
  @@log_file_path = "/tmp/ggs-log.csv"
  @@bot_threads = []
  
  def initialize(is_bot=false, table_token="")
    @is_bot = is_bot
    @log = nil
    @ignore = false
    
    stty_save = `stty -g`.chomp
    trap('INT') { system('stty', stty_save); exit }
    
    print "Table token (empty for new): " unless @is_bot
    table_token = gets.chomp unless @is_bot
    
    @ggs_network = GGSNetwork.new(self, table_token)
    @ggs_network.connect("localhost", 9000)
  end
  
  def ggsNetworkReady(ggs_network, am_i_host)    
    unless am_i_host
      say @ggs_network.table_token
      source_code = File.open("chat_server.js", "rb").read
      @ggs_network.define(source_code)
    else
      ggsNetworkDefined(ggs_network, true)
    end
  end
  
  def ggsNetworkDefined(ggs_network, defined)
    if defined
      @nick = ""
      while @nick == ""
        print "\rYour nickname: " unless @is_bot
        unless @is_bot
          @nick = gets.chomp
        else
          @nick = random_nick
        end
      end
      
      @ggs_network.sendCommand("/nick", @nick)
      
      t = Thread.new {
        loop do
          unless @is_bot
            input
          else
            sleep(rand 2) # interfall for bot to do something
            random_function
          end
        end
      }
      @@bot_threads << t if @is_bot
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
  
  def start_bots(number)
    number.times { |n|
      say "<starting bot #{n}>"
      @@bot_threads << Thread.new {
        Chat.new(true, @ggs_network.table_token)
      }
    } 
  end
  
  def stop_bots
    @@bot_threads.each do |bot|
      bot.kill
    end
    @bot_threads = []
    say "<stopped all bots>"
  end
  
  def message(message)
    say message
  end
  
  def notice(notice)
    say "<#{notice}>"
  end
  
  def input(message="")
    message = Readline.readline('> ', true) unless @is_bot
    
    if message[0,6] == "/nick "
      @nick = message[6..-1]
      @ggs_network.sendCommand("/nick", @nick)
    elsif message == "/ping"
      ping()
    elsif message[0,6] == "/bots "
      
      number = message[6..-1].to_i
      say "<starting #{number} bots>"
      start_bots(number)
    
    elsif message == "/bots"
      stop_bots
    elsif message == "/log"
      toggle_log
    elsif message == "/help"
      help
    elsif message == "/exit"
      exit
    elsif message == "/ignore"
      @ignore = @ignore ? false : true
      if @ignore
        say "<ignoring on>"
      else
        say "<ignoring off>"
      end
    else
      @ggs_network.sendCommand("message", message) unless message == ""
    end
  end
  
  def ping
    @start_ping = Time.now
    @ggs_network.sendCommand("ping", @ggs_network.player_token)
  end

  def pong(server_log)
    time = (Time.now - @start_ping).to_s
    say "<pong: #{time} #{server_log}>"
    
    File.open(@@log_file_path, 'a') {|f| f.write(time << ",#{server_log}\n") } unless @log.nil?
  end
  
  def say(something)
    unless @ignore or @is_bot
      puts "\r#{something}"
      print "> "
    end
    
  end
  
  def toggle_log
    if @log.nil?
      say "<starting logging>"
      @log = Thread.new {
        loop {
          sleep 1
          ping
        }
      }
    else
      @log.kill
      @log = nil
      say "<stopped logging>"
    end
  end
  
  def help
    puts "+-----------------------------------------------+"
    puts "| something        |  normal message            |"
    puts "| /nick something  |  changing your nick        |"
    puts "| /bots n          |  start n bots              |"
    puts "| /bots            |  stop all bots             |"
    puts "| /log             |  toggle logging            |"
    puts "| /ignore          |  toggle ignoring everyone  |"
    puts "| /exit            |  exit chat                 |"
    puts "| /help            |  show this help            |"
    puts "+-----------------------------------------------+"
  end
  
end

if __FILE__ == $0
  Chat.new    
end
