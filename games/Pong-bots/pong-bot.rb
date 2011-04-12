#!/usr/bin/env ruby -wKU

$: << "."

require 'ggs-network.rb'
require 'ggs-delegate.rb'

class PongBot
  include GGSDelegate
  
  def initialize
    @ggs_network = GGSNetwork.new(self)
  end
  
  def ggsNetworkReady(ggs_network, ready)
    @ggs_network.sendCommand("ready")
  end
  
  def ggsNetworkDefined(ggs_network, defined)
    # do nothing
  end
  
  def ggsNetworkReceivedCommandWithArgs(ggs_network, command, args)
    
  end

end

if __FILE__ == $0
    PongBot.new
end