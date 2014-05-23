make.burst.train <-
function(s){
  
  #make a burst train
  burst.train=list()
  burst.train[[length(s[[t.p]]$cw)]]=0
  for (cur.ch in c(1:length( s[[t.p]]$cw) ) ){
    
    temp= c()
    if (length(s[[t.p]]$allb[[cur.ch]][,1] )>0){
      for (burst.n in c(1: length(s[[t.p]]$allb[[cur.ch]][,1] ) ) ){
        
        firstSoB = s[[t.p]]$allb[[cur.ch]][burst.n,1]
        temp=c(temp, s[[t.p]]$spikes[[cur.ch]][firstSoB])
        
      }
      
    } else{
      burst.train[[cur.ch]]=NA
      
    }
    burst.train[[cur.ch]] = temp
    
  }
  burst.train
}
