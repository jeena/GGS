require 'socket'

class GGSNetwork
  
  SERVER = "Server"
  CLIENT = "Game"

  public
  
  attr_accessor :delegate

  def initialize(delegate)
    @delegate = delegate
  end
  
  def define(source_code)
    write( makeMessage(SERVER, "define", source_code) )
  end
  
  def sendCommand(command, args="")
    write( makeMessage(CLIENT, command, args) )
  end
  
  def connect(host='localhost', port=9000)
    @socket = TCPSocket.new(host, port)
    read
  end
  
  protected
  
  def write(message)
    @socket.write(message)
  end
  
  def read
    loop do
      headers = {}
      size = 0
      args = ""

      select([@socket], nil, nil)

      while (line = @socket.gets) != "\n"
        break if line.nil?
        
        key, value = line.split(": ")
        headers[key] = value.strip
      end

      if headers.has_key?("Content-Size")
        args = @socket.read(headers["Content-Size"].to_i)
      end

      receivedCommand(headers, args)
    end
  end
  
  def receivedCommand(headers, data)
    if headers.has_key? "Client-Command"
      command = headers["Client-Command"]
      case command
      when "hello"
        parse_hello(data)
        @delegate.ggsNetworkReady(self, @am_i_host)
      when "defined"
        @delegate.ggsNetworkDefined(self, true)
      else
        @delegate.ggsNetworkReceivedCommandWithArgs(self, command, data)
      end
    end
  end
  
  def makeMessage(serverOrGame, command, args)
    message = "Token: #{@game_token}\n" +
    "#{serverOrGame}-Command: #{command}\n" +
    "Content-Length: #{args.length}\n\n"

    message += args if args.length > 0

    message
  end
  
  def parse_hello(message)
    @game_token, shall_define, @table_token = message.split(",")
    @am_i_host = shall_define == "true"
  end
  
end
