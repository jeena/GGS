module MyRandom
  def random_function
    funcs = []
    funcs << lambda { ping() }
    funcs << lambda { input("/nick " + random_nick) }
    20.times { funcs << lambda { input(random_message) } }
    
    funcs[rnd(0,funcs.length)].call
  end
  
  def random_message
    random_string(rnd(1,30))
  end
  
  def random_nick
    random_string(rnd(1,6))
  end
  
  def random_string(length)
    o =  [('a'..'z'),('A'..'Z')].map{|i| i.to_a}.flatten;  
    (0..length).map{ o[rand(o.length)]  }.join;
  end
  
  def rnd(min, max)
    ((rand * (max - min)) + min).to_i
  end  
end