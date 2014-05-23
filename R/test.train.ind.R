test.train.ind <-
function(dig.s ){
  
  z.w = list()
  z.w[[choose(length(dig.s),2 ) ]] = c()
  
  temp.names = c() #names of comparissons
  
  cur.iter = 0 
  
  for (c.c in 1:(length(dig.s)-1) ){
    
    for (c.c2 in (c.c+1):length(dig.s) ){
      
      # the current time that we've been through loop
      cur.iter = cur.iter + 1 
      
      comp.ch=c(); 
      # 2 channels to compare
      comp.ch[1] = c.c; comp.ch[2]=c.c2 ; 
      names(comp.ch) = c( names(dig.s)[ comp.ch[1] ] , names(dig.s)[ comp.ch[2] ] )
      
      # this function takes 2 digitized spikes trains and makes a joint one with
      # 1 if both fire in same time bin, 0 otherwise
      joint_dig<-make.joint.digi.s( dig.s[[comp.ch[1] ]], dig.s[[comp.ch[2] ]] )
      
      
      # get y_dig rate
      p_joint = sum(joint_dig)/length(joint_dig) # 0.0004459943
      
      #get proportion under independence assumption
      p_0 =  rate[[comp.ch[1] ]]*rate[[comp.ch[2] ]]
      
      #test for equality of proportions, agains a null proportion p_0
      temp.z.w = wald.stat.1sample.prop(  p_0 , p_joint, length(joint_dig) )
      
      
      z.w[[cur.iter ]] = temp.z.w
      
      temp.names = c(temp.names, paste( names( dig.s)[c.c], "-", names(dig.s)[c.c2]))
      
    } #end of loop through 2nd coordinate
    
  } # loop through first spike train comparison
  
  names(z.w) = temp.names
  z.w #returns z.w
  
}
