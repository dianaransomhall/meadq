cv.timestamps <-
function(timestamps, s, t.p){
  
  #for (t.p in c(1:length(s)) ){

  
  
  #loop through wells
  cv_time=c() #cv_time for each well
  cv_network = c()  #cv_network for each network
  for (w in c(1:length(unique(s[[t.p]]$cw) ) ) ){
    
    #get indices of current well
    index=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w])
    
    #convert seconds to minutes (m), and round up 
    total.m = round( (s[[t.p]]$rec.time[2] - s[[t.p]]$rec.time[1])/60, 0)  #total minutes
    
    #cv_time:
    #loop through channels in current well
    # cv_Ch is a vector for each well
    cv_ch = c() 
    for ( cur.ch in index ){
      #loop through minutes in current channel
      temp.mean=c()
      cur.beg = s[[t.p]]$rec.time[1]
      for (cur.min in c(1:(total.m-1) ) ){
        cur.end = cur.beg + 60 #look at 60 second window of time
        
        #n.min is used to get mean
        n.min = 1
        if ( cur.min == total.m-1 ){
          n.min = (s[[t.p]]$rec.time[2]%%60)/60 #this is modulus operator so remainder
        }
        #beg=first spike (out of total spikes) that's in burst
        #end=last spike (of nspikes) that's in burst     
        #IBI len = time between current burst and previous burst    
        #durn= duration of a burst  
        #mean.isis = interval between spikes in burst
        #SI = surprise index
        # s[[1]]$spikes$F8_44[firstSpikeOfBurst], you can get time at which burst started by taking 'beg'
        # of current burst, firstSpikeOfBurst = s[[1]]$allb$F8_44[currentBurst,1]
        #per.spikes.out.burst mean.si   mean.isis sd.mean.isis mean.IBIs  sd.IBIs cv.IBIs
        temp.mean=c( temp.mean, 
                     sum( timestamps[[cur.ch]] <= cur.end & timestamps[[cur.ch]]>=cur.beg)  )/n.min
        cur.beg = cur.end #shift beginning of time window to end of last window
      } #end loop through minutes
      
      cv_ch = c( cv_ch, sd(temp.mean, na.rm=T )/mean(temp.mean, na.rm=T) )
      
    } # end of loop through index
    
    #cv_time is average cv well
    cv_time =  c(cv_time, mean( cv_ch, na.rm=T )  )
    
    
    
    
    
    
    #cv_network:
    #loop through channels in current well
    # cv_min is a vector for each well
    cv_min = c() 
    for (cur.min in c(1:(total.m-1) ) ) {
      #loop through minutes in current channel
      # temp.mean=c()
      cur.beg = s[[t.p]]$rec.time[1]
      cur.end = cur.beg + 60 #look at 60 second window of time
      
      
      #n.min is used to get mean
      n.min = 1
      if ( cur.min == total.m-1 ){
        n.min = (s[[t.p]]$rec.time[2]%%60)/60 #this is modulus operator so remainder
      }
      
      for ( cur.ch in index ){
        
        temp.mean=c( temp.mean, 
                     sum( timestamps[[cur.ch]] <= cur.end & timestamps[[cur.ch]]>=cur.beg)  )/n.min
        
      } #end loop through index
      
      cv_min = c( cv_min, sd(temp.mean, na.rm=T )/mean(temp.mean, na.rm=T) )
      
      cur.beg = cur.end #shift beginning of time window to end of last window
      
      
    } # end of loop through minutes
    
    #cv_time is average cv well
    cv_network =  c( cv_network, mean( cv_min, na.rm=T ) ) 
    
    
  }#end of loop through wells
  
  results = list( cv_time, cv_network)
  results
  
  # } #end of loop through time points
  
  
}
