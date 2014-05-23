cv.feature <-
function(feature, s){
  
  #for (t.p in c(1:length(s)) ){
  t.p=1
  
  
  #loop through wells
  cv_time=c() #cv_time for each well
  
  # loop through wells
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
      
      #only go through the channel if there's burst data
      if (  !(  is.null( dim( feature[[cur.ch]] ) ) )  ){
        
        #loop through minutes in current channel
        temp.mean=c()
        cur.beg = s[[t.p]]$rec.time[1]
        for (cur.min in c(1:(total.m-1) ) ){
          cur.end = cur.beg + 60 #look at 60 second window of time
          
          
          #beg=first spike (out of total spikes) that's in burst
          #end=last spike (of nspikes) that's in burst     
          #IBI len = time between current burst and previous burst    
          #durn= duration of a burst  
          #mean.isis = interval between spikes in burst
          #SI = surprise index
          # s[[1]]$spikes$F8_44[firstSpikeOfBurst], you can get time at which burst started by taking 'beg'
          # of current burst, firstSpikeOfBurst = s[[1]]$allb$F8_44[currentBurst,1]
          #per.spikes.out.burst mean.si   mean.isis sd.mean.isis mean.IBIs  sd.IBIs cv.IBIs
          index.t = (  feature[[cur.ch]][,1]<= cur.end & feature[[cur.ch]][,1]>=cur.beg)
          if (  sum(index.t)>0   ){
            
            temp.mean=c( temp.mean,  mean( feature[[cur.ch]][index.t, 2], na.rm=T)  ) 
          } # end if ( sum(index.t)>0  )
          cur.beg = cur.end #shift beginning of time window to end of last window
        } #end loop through minutes
        if (length(temp.mean)>1 ){
          cv_ch = c( cv_ch, sd(temp.mean, na.rm=T )/mean(temp.mean, na.rm=T) )
        } else{
          cv_ch = c( cv_ch, NA )
        }
        
      } else {
        
        cv_ch = c( cv_ch, NA )
      } #end of if (  !is.na(feature[[cur.ch]])  )
      
    } # end of cur.ch through index
    
    #cv_time is average cv well
    cv_time =  c(cv_time, mean( cv_ch, na.rm=T )  )
    
  }#end of loop through wells
  
  
  
  
  
  
  
  
  
  #cv_network:
  #loop through channels in current well
  # cv_min is a vector for each well
  cv_network = c()  #cv_network for each network
  for (w in c(1:length(unique(s[[t.p]]$cw) ) ) ){
    
    
    cv_min = c() 
    for (cur.min in c(1:(total.m-1) ) ) {
      #loop through minutes in current channel
      # temp.mean=c()
      cur.beg = s[[t.p]]$rec.time[1]
      cur.end = cur.beg + 60 #look at 60 second window of time
      
      
      for ( cur.ch in index ){
        
        
        
        if (  !(  is.null( dim( feature[[cur.ch]] ) ) )  ){
          index.t = (feature[[cur.ch]][,1] <= cur.end & feature[[cur.ch]][,1]>=cur.beg)
          temp.mean=c( temp.mean,  mean( feature[[cur.ch]][index.t, 2], na.rm=T)  ) 
          
        } 
        
        
      } #end loop through index
      
      if (!is.null(temp.mean) ){
        if (length(temp.mean)>1 ){
          cv_min = c( cv_min, sd(temp.mean, na.rm=T )/mean(temp.mean, na.rm=T) )
        } else{
          cv_min = c( cv_min, NA )
        }
        
        
      } else {
        cv_min = c( cv_min, NA )
      }
      
      
      
      
      cur.beg = cur.end #shift beginning of time window to end of last window
      
      
    } # end of loop through minutes
    
    
    #cv_time is average cv well
    cv_network =  c( cv_network, mean( cv_min, na.rm=T ) ) 
    
    
  }#end of loop through wells
  
  
  results = list( cv_time, cv_network)
  results
  
}
