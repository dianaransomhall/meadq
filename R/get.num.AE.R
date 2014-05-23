get.num.AE <-
function(s2){
  #add number of active electrodes
  s2$nAE<-rep(0,length(s2$well))
  names(s2$nAE)<-s2$well
  for (i in 1:s2$NCells){
    s2$nAE[which(substr(s2$channels[i],1,2)==(s2$well))]=
      s2$nAE[which(substr(s2$channels[i],1,2)==(s2$well))]+1
    
    s2$cw[i]<-substr(s2$channels[i],1,2)
  }
  
  s2 
  
  
}
