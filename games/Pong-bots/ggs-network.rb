require 'socket'

class GGSNetwork
  
  SERVER = "Server"
  CLIENT = "Client"

  public
  
  attr_accessor :delegate

  def initialize(delegate, host='localhost', port=9000)
    connect(host, port)
    @delegate = delegate
  end
  
  def define(source_code)
    write( makeMessage(SERVER, "define", source_code) )
  end
  
  def sendCommand(command, args="")
    write( makeMessage(CLIENT, command, args) )
  end
  
  protected
  
  def connect(host, port)
    gs = TCPSocket.open(host, port)
    @socket = gs.accept
    Thread.start(@socket) { |s| read(s) }
  end
  
  def write(message)
    @socket.write(message)
  end
  
  def read(s)
    loop do
      headers = {}
      size = 0
      args = ""

      while (line = @socket.gets) != "\n"
        key, value = line.split(": ")
        headers[key] = value
      end

      if headers.contains?("Content-Size")
        headers["Content-Size"].to_i.times do
          args << @socket.recv
        end
      end

      receivedCommand(headers, args)          
    end
  end
  
  def receivedCommand(headers, data)
    if headers.contains? "Client-Command"
      command = headers["Client-Command"]
      case command
      when "hello"
        @delegate.ggsNetworkReady(self, true)
      when "defined"
        @delegate.ggsNetworkDefined(self, true)
      else
        @delegate.ggsNetworkReceivedCommandWithArgs(self, command, data)
      end
    end
  end
  
  def makeMessage(serverOrGame, command, args)
    message =<<MESSAGE
Token: #{@game_t oken}
#{serverOrGame}-Command: #{command}
Content-Length: #{args.length}

#{args}
MESSAGE
    message
  end
  
end
