filter.spikes<-function(h5Files,
                        elec.min.rate=(1/60), 
                        elec.max.rate=25,
                        well.min.rate=15){
  # rates are in Hz
  result <- list()
  count <- 0
  for (i in 1:length(h5Files)){
    if (!(i==1)){
      rm(s1,s2)
    }
    s1<-h5.read.spikes(h5Files[i], beg=NULL, end=NULL)
    
    #indices of low and high firing rate
    low <- which(s1$meanfiringrate < elec.min.rate)
    high <- which(s1$meanfiringrate > elec.max.rate)
    
    ## TODO, check that low and high are non-zero length vectors.
    extremes <- c(low, high)
    
    bad.ids <- names(extremes)
    bad.ids <- c("-", bad.ids)  # "-" needed to remove these ids!
    
    s2 <- remove.spikes(s1, bad.ids)
    
    s2<-well.info(s2)
    
    #indices of low and high firing rate
    
    low <- which(s2$nAE < well.min.rate)
    
    bad.wells <- names(low)
    bad.wells <- c("-", bad.wells)   # "-" needed to remove these well!
    #just these three for example
    s<- remove.spikes(s2, bad.wells)
    s<-well.info(s)
    s$goodwells<-names(which(s2$nAE >= well.min.rate))
    if (s$meanfiringrate[1] > 0) {
      count <- count + 1
      result[[count]] <- s
    }
  }#end of filter loop through folder
  result
}#end of filter.spikes function

