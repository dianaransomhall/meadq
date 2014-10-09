filter.spikes.dh<-function(h5Files,
                           elec.min.rate=(1/60), 
                           elec.max.rate=25 ,
                           well.min.rate=15 ){
  # rates are in Hz
  for (i in 1:length(h5Files)){
    if (!(i==1)){
      rm(s1,s2)
    }
    s1<-h5.read.spikes.dh(h5Files[i], beg= NULL, end=NULL)
    
    #indices of low and high firing rate
    low <- which(s1$meanfiringrate < elec.min.rate)
    high <- which(s1$meanfiringrate > elec.max.rate)
    
    ## TODO, check that low and high are non-zero length vectors.
    extremes <- c(low, high)
    
    bad.ids <- names(extremes)
    bad.ids <- c("-", bad.ids)  # "-" needed to remove these ids!
    
    s2 <- remove.spikes(s1, bad.ids)
    
    s1.names<-names(s1)
    
    if ( is.element("treatment", s1.names) ){
      s2$treatment<-s1$treatment
    }
    if ( is.element("genotype", s1.names) ){
      s2$genotype<-s1$genotype
    }
    if ( is.element("pup", s1.names) ){
      s2$pup<-s1$pup
    }
    if ( is.element("trt.div", s1.names) ){
      s2$trt.div<-s1$trt.div
    }
    if ( is.element("cw", s1.names) ){
      s2$cw<-s1$cw
    }
    if ( is.element("rec.time", s1.names) ){
      s2$rec.time<-s1$rec.time
    }
    if ( is.element("units", s1.names) ){
      s2$units<-s1$units
    }
    if ( is.element("dose", s1.names) ){
      s2$dose<-s1$dose
    }
    

    s2$well<-s1$well
    
    #get.num.AE
    s2<-get.num.AE(s2)
    
    #indices of low and high firing rate
    
    low <- which(s2$nAE < well.min.rate)
    
    bad.wells <- names(low)
    bad.wells <- c("-", bad.wells)   # "-" needed to remove these well!
    #just these three for example
    s<- remove.spikes(s2, bad.wells)
    
    s$goodwells<-names(which(s2$nAE >= well.min.rate))
    
    
    s$well<-s1$well
    if ( is.element("treatment", s1.names) ){
      s$treatment<-s1$treatment
      names(s$treatment)<-s1$well
    }
    if ( is.element("genotype", s1.names) ){
      s$genotype<-s1$genotype
      names(s$genotype)<-s1$well
    }
    if ( is.element("pup", s1.names) ){
      s$pup<-s1$pup
      names(s$pup)<-s1$well
    }
    if ( is.element("trt.div", s1.names) ){
      s$trt.div<-s1$trt.div
      names(s$trt.div)<-s$trt.div
    }

    if ( is.element("rec.time", s1.names) ){
      s$rec.time<-s1$rec.time
    }
    if ( is.element("units", s1.names) ){
      s$units<-s1$units
      names(s$units)<-s1$well
    }
    if ( is.element("dose", s1.names) ){
      s$dose<-s1$dose
      names(s$dose)<-s1$well
    }
    
    s$cw<-substring(s$channels,1,2)
    
    s<-get.num.AE(s)
    
  }#end of filter loop through folder
  s
}#end of filter.spikes.dh function
