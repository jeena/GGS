module GGSDelegate
  
  def ggsNetworkReady(ggs_network, ready)
    raise "ggsNetworkReady(ggs_network, ready) must be overridden"
  end
  
  def ggsNetworkDefined(ggs_network, defined)
    raise "ggsNetworkDefined(ggs_network, defined) must be overridden"
  end
  
  def ggsNetworkReceivedCommandWithArgs(ggs_network, command, args)
    raise "ggsNetworkReceivedCommandWithArgs(ggs_network, command, args) must be overridden"
  end
    
end