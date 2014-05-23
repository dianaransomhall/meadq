

get.burst.info.averaged.over.well <-function(s, goodwells.ind=TRUE ){  
  
  
  sum=list()#summary for each timepoint
  masterSum<-list()#summary over all files
  for (i in 1:length(s)){   
    #calculate bursting variables for current h5File
    nbursts <- sapply(s[[i]]$allb, nrow) 
    allb<-s[[i]]$allb
    tempsum <- calc.burst.summary(s[[i]])
    
    #ISIs: gets the ISI for each channel of s[[i]]
    ISIs = calc.all.isi(s[[i]], allb)
    
    #IBIs get IBI's across all inter burst intervals across all data
    tempIBIs<-calc.all.ibi(s[[i]],allb)
    
    #loop through goodwells
    # goodwells.ind=T if you want to filter by good wells
    if (goodwells.ind ){
      
      for (j in 1:length(s[[i]]$goodwells)){
      #indicator of current well

        icurrentwell<-(s[[i]]$goodwells[j]==s[[i]]$cw)
        
        #index of current well
        incurrentwell<-which(s[[i]]$goodwells[j]==s[[i]]$cw)
        
        if (sum(icurrentwell)!=0){       
        #####variables that need summing and averaging  
        #total spikes across all AE in current well
        sum$nspikes[j]<-sum(tempsum$spikes[icurrentwell], na.rm=TRUE)
        
        #Total recorded time on current well= recording time * nAE
        sum$duration[j]<-length(incurrentwell)*(s[[i]]$rec.time[2]-s[[i]]$rec.time[1])
        
        #mean duration
        sum$mean.dur[j]<-mean(tempsum$mean.dur[incurrentwell],na.rm=TRUE)
        
        #mean spikes per second
        sum$mean.freq[j]<-mean(tempsum$mean.freq[incurrentwell],na.rm=TRUE)
        #total number of bursts
        sum$nbursts[j]<-sum(tempsum$nbursts[icurrentwell], na.rm=TRUE)
        #mean burst per second
        sum$bursts.per.sec[j]<-mean(tempsum$bursts.per.sec[incurrentwell])
        #mean burst per minute
        sum$bursts.per.min[j]<-sum$bursts.per.sec[j]*60
        
        #finds the mean of duration for a particular well (across all channels)
        #burstinfo(allb[icurrentwell],"durn") takes out the column "durn" of all
        #matricies allb among the indicator set icurrentwell
        #get duration data across all channels of current well
        sum$mean.dur[j]<-mean(unlist(burstinfo(allb[icurrentwell],"durn")),na.rm=TRUE)
        
        #sd of current well burst durations
        sum$sd.dur[j]<-sd(unlist(burstinfo(allb[icurrentwell],"durn")))
        
        #mean frequency within a burst
        sum$mean.freq.in.burst[j]<-
          mean(unlist(burstinfo(allb[incurrentwell],"len"))/
                 unlist(burstinfo(allb[incurrentwell],"durn")), na.rm=TRUE)
        
        #sd frequency within a burst
        sum$sd.freq.in.burst[j]<-sd(unlist(burstinfo(allb[incurrentwell],"len"))/
                                      unlist(burstinfo(allb[incurrentwell],"durn")),na.rm=TRUE)
        
        #mean of ISI across all channels in current well
        sum$mean.ISIs[j] = mean(unlist(ISIs[incurrentwell]), na.rm=TRUE)
        
        #finds sd of ISI across all channels in current well
        sum$sd.ISIs[j] = sd( unlist(ISIs[incurrentwell]), na.rm = TRUE)
        
        #len=#spikes in burst (length of burst in bursts)
        #mean.spikes.in.burst
        ns<-unlist(burstinfo(allb[icurrentwell],"len"))   
        sum$mean.spikes.in.burst[j] <- round(mean(ns,na.rm=TRUE), 3)
        
        #sd of spikes in burst
        sum$sd.spikes.in.burst[j] <- round(sd(ns,na.rm=TRUE), 3)
        
        #total number of spikes arcross all bursts
        sum$total.spikes.in.burst[j] <- sum(ns, na.rm=TRUE)
        
        #percent of spikes in bursts
        sum$per.spikes.in.burst[j] <- 
          round(100 * (sum$total.spikes.in.burst[j]/sum$nspikes[j]),3)
        
        #mean IBI
        sum$mean.IBIs[j]<-round(mean(unlist(tempIBIs[incurrentwell]),na.rm=TRUE),3)
        #sd IBI
        sum$sd.IBIs[j]<-round(sd(unlist(tempIBIs[incurrentwell]),na.rm=TRUE),3)
        #cv IBI
        sum$cv.IBIs[j]<-round(sum$mean.IBIs[j]/sum$sd.IBIs[j],3)
      } else {
        #if well has no channels in the current file, then sum=NA
        for (f in 1:length(sum)){
          sum[[f]][j]=NA
        }#end of for loop
      }# end of if/else for channels that dropped off
      
      
    }#end of loop through goodwells
    
    } else{
      
      wells<- unique(s[[i]]$cw ) 
      
      for (j in 1:length( wells ) ){
        
        #indicator of current well        
        icurrentwell = ( wells[j] == s[[i]]$cw )
        
        #index of current well
        incurrentwell<-which(wells[j]==s[[i]]$cw)
        
        if ( sum( icurrentwell )!=0 ){       
          #####variables that need summing and averaging  
          #total spikes across all AE in current well
          sum$nspikes[j]<-sum(tempsum$spikes[icurrentwell], na.rm=TRUE)
          
          #Total recorded time on current well= recording time * nAE
          sum$duration[j]<-length(incurrentwell)*(s[[i]]$rec.time[2]-s[[i]]$rec.time[1])
          
          #mean duration
          sum$mean.dur[j]<-mean(tempsum$mean.dur[incurrentwell],na.rm=TRUE)
          
          #mean spikes per second
          sum$mean.freq[j]<-mean(tempsum$mean.freq[incurrentwell],na.rm=TRUE)
          #total number of bursts
          sum$nbursts[j]<-sum(tempsum$nbursts[icurrentwell], na.rm=TRUE)
          #mean burst per second
          sum$bursts.per.sec[j]<-mean(tempsum$bursts.per.sec[incurrentwell])
          #mean burst per minute
          sum$bursts.per.min[j]<-sum$bursts.per.sec[j]*60
          
          #finds the mean of duration for a particular well (across all channels)
          #burstinfo(allb[icurrentwell],"durn") takes out the column "durn" of all
          #matricies allb among the indicator set icurrentwell
          #get duration data across all channels of current well
          sum$mean.dur[j]<-mean(unlist(burstinfo(allb[icurrentwell],"durn")),na.rm=TRUE)
          
          #sd of current well burst durations
          sum$sd.dur[j]<-sd(unlist(burstinfo(allb[icurrentwell],"durn")))
          
          #mean frequency within a burst
          sum$mean.freq.in.burst[j]<-
            mean(unlist(burstinfo(allb[incurrentwell],"len"))/
                   unlist(burstinfo(allb[incurrentwell],"durn")), na.rm=TRUE)
          
          #sd frequency within a burst
          sum$sd.freq.in.burst[j]<-sd(unlist(burstinfo(allb[incurrentwell],"len"))/
                                        unlist(burstinfo(allb[incurrentwell],"durn")),na.rm=TRUE)
          
          #mean of ISI across all channels in current well
          sum$mean.ISIs[j] = mean(unlist(ISIs[incurrentwell]), na.rm=TRUE)
          
          #finds sd of ISI across all channels in current well
          sum$sd.ISIs[j] = sd( unlist(ISIs[incurrentwell]), na.rm = TRUE)
          
          #len=#spikes in burst (length of burst in bursts)
          #mean.spikes.in.burst
          ns<-unlist(burstinfo(allb[icurrentwell],"len"))   
          sum$mean.spikes.in.burst[j] <- round(mean(ns,na.rm=TRUE), 3)
          
          #sd of spikes in burst
          sum$sd.spikes.in.burst[j] <- round(sd(ns,na.rm=TRUE), 3)
          
          #total number of spikes arcross all bursts
          sum$total.spikes.in.burst[j] <- sum(ns, na.rm=TRUE)
          
          #percent of spikes in bursts
          sum$per.spikes.in.burst[j] <- 
            round(100 * (sum$total.spikes.in.burst[j]/sum$nspikes[j]),3)
          
          #mean IBI
          sum$mean.IBIs[j]<-round(mean(unlist(tempIBIs[incurrentwell]),na.rm=TRUE),3)
          #sd IBI
          sum$sd.IBIs[j]<-round(sd(unlist(tempIBIs[incurrentwell]),na.rm=TRUE),3)
          #cv IBI
          sum$cv.IBIs[j]<-round(sum$mean.IBIs[j]/sum$sd.IBIs[j],3)
        } else {
          #if well has no channels in the current file, then sum=NA
          for (f in 1:length(sum) ){
            sum[[f]][j]=NA
          }#end of for loop
        }# end of if/else for channels that dropped off
        
        
      }#end of loop through all wells   
      
    } # end of if well or goodwell

    
    ###Set all names
    if ( goodwells.ind ) {
      for (k in 1:length(names(sum))){ names(sum[[k]])=s[[i]]$goodwells } #end of for 
      #make a masterSum, that is a list of all the summaries
      goodwellindex<-which(is.element(s[[i]]$well, s[[i]]$goodwells ) )
      num.reps<-length( goodwellindex )
      goodwells<-s[[i]]$goodwells
      
    } else {
      for (k in 1:length( names(sum )) ){names(sum[[k]])= wells }   
      #make a masterSum, that is a list of all the summaries
      goodwellindex<-wells
      num.reps<-length( goodwellindex )
      goodwells<-wells
    } # end of if/else
 
    masterSum[[i]]<-sum
    masterSum[[i]]$file<-strsplit(basename(s[[i]]$file),".h5")[[1]][1]
    masterSum[[i]]$treatment<-s[[i]]$treatment[goodwellindex]
    masterSum[[i]]$size=s[[i]]$size[goodwellindex]
    masterSum[[i]]$dose=s[[i]]$dose[goodwellindex]
    masterSum[[i]]$well<-s[[i]]$well[goodwellindex]
    masterSum[[i]]$nAE<-s[[i]]$nAE[goodwellindex]
    masterSum[[i]]$timepoint=rep(s[[i]]$timepoint[1], num.reps)
    masterSum[[i]]$start.rec.time<-rep( s[[i]]$rec.time[1], num.reps )
    masterSum[[i]]$end.rec.time<-rep( s[[i]]$rec.time[2], num.reps )
    masterSum[[i]]$goodwells<-s[[i]]$goodwells
    
    
  }#end of for loop through sum/averaging burst variables
  
  masterSum
}
