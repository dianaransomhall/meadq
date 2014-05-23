local.corr.by.channel <-
function(s) {
  
  # store data
  plate.corr=list(plate.name=list(), local.cor=list())
  
  #loop through different time points
  #t.p is time points
  for (t.p in c(1:length(s))){
    
    
    
    #now get correlations within well
    temp=list()
    r.vector=list()
    r.vector[[length( unique(s[[t.p]]$cw) )]]=0
    
    names(r.vector)=unique(s[[t.p]]$cw)
    
    #using  (Demas 2009), figure 5, closer neurons are more correlated
    #therefore I'm going to measure only cor. btw adjacent electrodes
    
    #loop through wells
    for (w in c(1:length(unique(s[[t.p]]$cw))) ){
      
      #get indices of current well
      index.w=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w])
      
      #convert seconds to ms
      start.ms=round(s[[t.p]]$rec.time[1]*10^2,digits=0)
      stop.ms=round(s[[t.p]]$rec.time[2]*10^2,digits=0)
      total.ms=stop.ms-start.ms
      
      
      #digitize spikes
      dig.s=list()
      dig.s[[length(index.w)]]=rep(0, total.ms )
      
      
      #digitize spikes: loop through channels
      for (i in c(1:length(index.w))){
        
        dig.s[[i]]=rep(0, total.ms)
        current.s.ms=round(s[[t.p]]$spikes[[index.w[i] ]]*10^2, digits=0)- start.ms 
        for (j in current.s.ms ){
          #get millisecond
          dig.s[[i]][ j ] = 1
        }
      }
      
      
      
      #get indices of current well
      index.c=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w])
      
      #get channels
      w.rows=as.numeric( substr(s[[t.p]]$channels[index.c],4,4) )
      w.cols=as.numeric( substr(s[[t.p]]$channels[index.c],5,5) )
      coords=rbind(w.rows,w.cols)
      
      #initialize c.vector: vector of mean(r), for each channel in current well
      c.vector = c()
      
      #loop through channels, getting cor between an electrode
      #and electrodes above, below, to left and to right
      # if those electrodes have AE
      for (i in c(1:length(s[[t.p]]$channels[index.c] ) ) ){
        
        #c.c is current coordinates of electrode on plate map
        c.c=coords[,i]
        temp.corr= c()
        
        #check for suitable coordinates to coorelate with       
        #example: take c.c = (4,4)
        # (c.c[1]==coords[1,] & c.c[2]+1==coords[2,]) means:
        # is current channel row (c.c[1]=4) same as and row in other electodes,
        # and current column (c.c[2]=4), +1, =5 the same as any column
        # this would correspond to an electrode (4,5). none exists so if loop not entered
        
        if (sum(c.c[1]==coords[1,] & c.c[2]+1==coords[2,])>0){
          # (4,5)
          ind.c.to.comp=which(c.c[1]==coords[1,] & c.c[2]+1==coords[2,])
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum( c.c[1]==coords[1,] & c.c[2]-1==coords[2,])>0){
          #this would be (4,3),
          ind.c.to.comp= which(c.c[1]==coords[1,] & c.c[2]-1==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum(c.c[1]-1==coords[1,] & c.c[2]==coords[2,])>0){
          # this would be (3,4), so no match in 48 well plate
          ind.c.to.comp= which(c.c[1]-1==coords[1,] & c.c[2]==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum(c.c[1]+1==coords[1,] & c.c[2]==coords[2,])>0){
          # this would be (5,4)
          ind.c.to.comp=which(c.c[1]+1==coords[1,] & c.c[2]==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum(c.c[1]+1==coords[1,] & c.c[2]+1==coords[2,])>0){
          # this would be (5,5)
          ind.c.to.comp=which(c.c[1]+1==coords[1,] & c.c[2]+1==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum(c.c[1]-1==coords[1,] & c.c[2]-1==coords[2,])>0){
          # this would be (3,3)
          ind.c.to.comp=which(c.c[1]-1==coords[1,] & c.c[2]-1==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum(c.c[1]+1==coords[1,] & c.c[2]-1==coords[2,])>0){
          # this would be (5,3)
          ind.c.to.comp=which(c.c[1]+1==coords[1,] & c.c[2]-1==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        if (sum(c.c[1]-1==coords[1,] & c.c[2]+1==coords[2,])>0){
          # this would be (3,5)
          ind.c.to.comp=which(c.c[1]-1==coords[1,] & c.c[2]+1==coords[2,]) 
          temp.corr=c(temp.corr, cor(dig.s[[i]], dig.s[[ind.c.to.comp]]) )
        }
        #vector of correlations inside well
        c.vector = c(c.vector, mean(temp.corr, na.rm=T ) )
        
      }#end of loop through channels
      names(c.vector)= s[[t.p]]$channels[index.c]
      r.vector[[w]] = c.vector
      
    }#end of loop through wells
    
    plate.corr$plate.name[t.p]=strsplit(basename(s[[t.p]]$file),'.h5')[[1]][1]
    
    plate.corr$local.cor[[t.p]]=r.vector
    
    
    
  }#end of loop through time points
  plate.corr # returns plate.corr
  
}
