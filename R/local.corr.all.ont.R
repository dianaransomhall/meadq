local.corr.all.ont <-
function(s, t.p) {
  
  # store data
  
  well.list<-unique(s[[t.p]]$cw)
  
  temp=list()
  #r.vector is a list for each channel
  r.vector=list()
  r.vector[[length( well.list )]]=0
  
  names(r.vector)= well.list
  
  #loop through wells
  for (cur.well in well.list ){
    
    #get indices of channels within current well
    index.ch=which(s[[t.p]]$cw==cur.well)
    
    #convert seconds to ms
    start.ms=round(s[[t.p]]$rec.time[1]*10^2,digits=0)
    stop.ms=round(s[[t.p]]$rec.time[2]*10^2,digits=0)
    total.ms=stop.ms-start.ms
    
    #digitize spikes
    dig.s=list()
    dig.s[[length(index.ch)]]=rep(0, total.ms )      
    
    #digitize spikes: loop through current channels
    #dig.s[[i]] is a list 1:# channels within current well
    for (cur.index in c(1:length(index.ch)) ){
      
      cur.ch = index.ch[cur.index]
      
      dig.s[[cur.index]]=rep(0, total.ms)
      
      current.s.ms=round(s[[t.p]]$spikes[[cur.ch]]*10^2, digits=0)- start.ms 
      for (j in current.s.ms ){
        #get millisecond
        dig.s[[cur.index]][ j ] = 1
      }
    } #end loop through current channels 
    
    
    #initialize c.vector: vector of mean(r), for each channel in current well
    
    
    #loop through channels in current well, getting cor between an electrode
    #and electrodes above, below, to left and to right
    # if those electrodes have AE
    c.vector = c(); ch.names=c();
    for (cur.ch in c(1:length(index.ch) ) ){
      
      temp.corr= c()
      for (other.ch in setdiff(c(1:length(index.ch )),cur.ch)  ){
        temp.corr=c(temp.corr, cor(dig.s[[cur.ch]], dig.s[[other.ch]]) )
        
      } #end of loop through other channels       
      
      #vector of mean correlations inside well by channel
      c.vector = c(c.vector, mean(temp.corr, na.rm=T ) )
      
    }#end of loop through channels
    
    names(c.vector)= s[[t.p]]$channels[index.ch]
    # each well get one vector
    r.vector[[cur.well]] = c.vector
    
  }#end of loop through wells
  
  r.vector   
  
}
