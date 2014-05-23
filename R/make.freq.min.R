make.freq.min <-
function( s ){
 
  
for (t.p in 1:length(s) ){
  # min.freq.ch is a vector for each channel of min to min firing rates
  min.freq.ch = c()   
  
  # min.freq.sd is sd of min.freq.ch
  min.freq.ch.sd = c() 

  min.freq.well = c() #firing rate for each well
  
  min.freq.well.sd = c() #firing rate of each well

  # loop through each well
  for (w in c(1:length(unique(s[[t.p]]$cw) ) ) ){
    
    #get indices of current well
    index=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w])
    
    #convert seconds to minutes (m), and round up 
    total.m = round( (s[[t.p]]$rec.time[2] - s[[t.p]]$rec.time[1])/60, 0)  #total minutes   

    #loop through channels in current well

    temp.ch.mean = c()
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

        temp.mean=c( temp.mean, 
                     sum( s[[t.p]]$spikes[[cur.ch]] <= cur.end & s[[t.p]]$spikes[[cur.ch]]>=cur.beg)/n.min )
        cur.beg = cur.end #shift beginning of time window to end of last window
      } #end loop through minutes
      
      min.freq.ch[[cur.ch]] = temp.mean 
      
      # sd 
      min.freq.ch.sd[[cur.ch]] = sd(temp.mean, na.rm = T )
      
      temp.ch.mean = c( temp.ch.mean, mean(min.freq.ch[[cur.ch]], na.rm=T ) )
      
    } # end of loop through index, through current well
    
    
    temp.well.mean = mean( temp.ch.mean, na.rm = T )
    # min.freq.well average min.freq.ch per well
    min.freq.well  =  c(min.freq.well, temp.well.mean  )
    
    # min.freq.well sd mean( min.freq.ch ) per well
    min.freq.well.sd  =  c(min.freq.well.sd, sd(temp.ch.mean, na.rm=T  ) )
    
  } # end of loop through wells
  
  # names !
  names(min.freq.ch) = names( s[[t.p]]$meanfiringrate)   

  names(min.freq.ch.sd) = names( s[[t.p]]$meanfiringrate) 
  
  names( min.freq.well ) = unique(s[[t.p]]$cw)
  
  names( min.freq.well.sd ) = unique(s[[t.p]]$cw)

  s[[t.p]]$min.freq.ch = min.freq.ch
  s[[t.p]]$min.freq.ch.sd = min.freq.ch.sd
  s[[t.p]]$min.freq.well = min.freq.well
  s[[t.p]]$min.freq.well.sd = min.freq.well.sd
  
} # end of for( t.p in 1:length(s) )
# return s
s
  
}
