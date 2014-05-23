test.connect.change <-
function(dig.s1, dig.s2  ){
  
  z.w = list()
  z.w[[choose(length(dig.s1),2 ) ]] = c()
  
  cur.iter = 0 # keep track of loop iterations
  temp.names = c() #keep track of names
  
  for (c.c in 1:(length(dig.s1)-1) ){
    
    for (c.c2 in (c.c+1):length(dig.s1) ){   
      
      if (!(c.c==1 & c.c2==c.c+1)){
        rm(joint_s1, joint_s2)
      }
      
      
      # increment loop counter
      cur.iter = cur.iter + 1 
      
      #make joint spikes 1
      joint_s1<-make.joint.digi.s( dig.s1[[c.c]], dig.s1[[c.c2 ]] )   
      
      #make joint spikes 2
      joint_s2<-make.joint.digi.s( dig.s2[[c.c]], dig.s2[[c.c2 ]] )
      
      
      # get number of joint spikes in each
      nsucess1 = sum(joint_s1) ;  nsucess2 = sum(joint_s2)   
      
      n1 = length(joint_s1); n2 = length(joint_s2)
      
      #test for equality of proportions, agains a null proportion p_0
      temp.z.w = wald.stat.2sample.prop(nsucess1, nsucess2, n1, n2)
      
      
      z.w[[cur.iter]] = temp.z.w
      
      #temp names
      temp.names = c(temp.names, paste( names( dig.s)[c.c], "-", names(dig.s)[c.c2]))
      
    } #end of loop through 2nd coordinate
    
  } # loop through first spike train comparison
  
  names(z.w) = temp.names
  z.w #return z.w
  
}
