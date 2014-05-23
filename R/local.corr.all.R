local.corr.all <-
function(s) {
  
  # store data
  plate.corr=list(plate.name=list(), local.cor=list())
  
  #loop through different time points
  #t.p is time points
  for (t.p in c(1:length(s))){
    
    
    
    temp=list()
    #r.vector is a list for each channel
    r.vector=list()
    r.vector[[length( unique(s[[t.p]]$cw) )]]=0
    
    names(r.vector)=unique(s[[t.p]]$cw)
    
    #using  (Demas 2009), figure 5, closer neurons are more correlated
    #therefore I'm going to measure only cor. btw adjacent electrodes
    
    #loop through wells
    for (w in c(1:length(unique(s[[t.p]]$cw))) ){
      
      #get indices of channels within current well
      index.w=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w])
      
      #convert seconds to ms
      start.ms=round(s[[t.p]]$rec.time[1]*10^2,digits=0)
      stop.ms=round(s[[t.p]]$rec.time[2]*10^2,digits=0)
      total.ms=stop.ms-start.ms
      
      
      #digitize spikes
      dig.s=list()
      dig.s[[length(index.w)]]=rep(0, total.ms )
      
      
      #digitize spikes: loop through current channels
      #dig.s[[i]] is a list 1:# channels within current well
      for (i in c(1:length(index.w))){
        
        dig.s[[i]]=rep(0, total.ms)
        current.s.ms=round(s[[t.p]]$spikes[[index.w[i] ]]*10^2, digits=0)- start.ms 
        for (j in current.s.ms ){
          #get millisecond
          dig.s[[i]][ j ] = 1
        }
      } #end loop through current channels 
      
      
      #get indices of current well
      index.c=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w])
      
      #initialize c.vector: vector of mean(r), for each channel in current well
      c.vector = c()
      
      #loop through channels in current well, getting cor between an electrode
      #and electrodes above, below, to left and to right
      # if those electrodes have AE
      for (curCh in c(1:length(s[[t.p]]$channels[index.c] ) ) ){
        
        #c.c is current coordinates of electrode on plate map
        temp.corr= c()
        
        #loop through channels other than current one
        for (otherCh in setdiff(c(1:length(s[[t.p]]$channels[index.c] )),curCh)  ){
          temp.corr=c(temp.corr, cor(dig.s[[curCh]], dig.s[[otherCh]]) )
          
        } #end of loop through other channels
        
        
        #vector of mean correlations inside well by channel
        c.vector = c(c.vector, mean(temp.corr, na.rm=T ) )
        
      }#end of loop through channels
      names(c.vector)= s[[t.p]]$channels[index.c]
      # each well get one vector
      r.vector[[w]] = c.vector
      
    }#end of loop through wells
    
    plate.corr$plate.name[t.p]=strsplit(basename(s[[t.p]]$file),'.h5')[[1]][1]
    
    plate.corr$local.cor[[t.p]]=r.vector
    
    
    
  }#end of loop through time points
  plate.corr # returns plate.corr
  
}
