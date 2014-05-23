filter.spikes.burst.info <-
function(h5Files,user.defined.goodwells=c("GG"), beg=180){
  
  h5Files<-sort(h5Files)
  
  mi.par <- list(beg.isi =    0.1,
                 end.isi =    0.25,
                 min.ibi =    0.8,
                 min.durn =   0.05,
                 min.spikes = 5)
  s=list()
  i=1
  ##obtain data from all h5Files
  for (i in 1:length(h5Files)){
    
    timepoint<-substring(basename(h5Files[i]),
                         nchar(basename(h5Files[i]))-8,
                         nchar(basename(h5Files[i]))-7)
    # if timepoint=="00" then remove inactive wells and spikes
    if (timepoint=="00"){
      s[[i]]<-filter.spikes.dh(h5Files[i], beg=beg)
      goodwells<-s[[i]]$goodwells
      s[[i]]$timepoint<-timepoint
    } else{
      temp<-h5.read.spikes.dh(h5Files[i], beg=180, end=NULL)
      #set min, max rate 
      elec.min.rate=5/60
      elec.max.rate=25
      
      #indices of low and high firing rate
      low <- which(temp$meanfiringrate < elec.min.rate)
      high <- which(temp$meanfiringrate > elec.max.rate)
      #get bad wells from "00" time point, or if no "00" timepoint given,
      #then calculate goodwells
      if (exists("goodwells")){
        bad.wells<- temp$well[which(!(is.element(temp$well,goodwells))) ]
      } else if (user.defined.goodwells[1]!="GG"){
        goodwells<-user.defined.goodwells
        bad.wells<- temp$well[which(!(is.element(temp$well,user.defined.goodwells))) ]
      } else{
        print("NO time point =00 or baseline given")
        print("Calcuting bad.wells< minimum number of active electrodes per well")
        if (length(temp$well)==12){
          min.AE.per.well=10
        } else{
          min.AE.per.well=16
        }
        temp.nAE<-get.num.AE(temp)
        bad.wells<- temp$well[which(temp.nAE$nAE<min.AE.per.well) ]
        goodwells<-temp$well[which(temp.nAE$nAE >= min.AE.per.well) ]
      }#end of else, for !exists("goodwells")
      #make list of channels in bad wells to exclude
      channels.in.bad.wells<-temp$channels[ which(is.element(temp$cw,bad.wells) ) ]
      
      ## TODO, check that low and high are non-zero length vectors.
      extremes <- c(low, high)
      #bad.ids are the union of low and high firing channels and the channels from
      #wells dead in timepoint="00"
      bad.ids <- union(names(extremes),channels.in.bad.wells)
      bad.ids <- c("-", bad.ids)  # "-" needed to remove these ids!
      
      s[[i]] <- remove.spikes(temp, bad.ids)
      
      #[which(!(is.element(temp$well,bad.wells)))]
      s[[i]]$goodwells<-goodwells
      s[[i]]$timepoint<-timepoint
      s[[i]]$file<-strsplit(basename(s[[i]]$file),".h5")[[1]][1]
      s[[i]]$treatment<-temp$treatment
        names(s[[i]]$treatment)<-temp$well
      s[[i]]$size=temp$size
        names(s[[i]]$size) <-temp$well
      s[[i]]$dose<-temp$dose
        names(s[[i]]$dose)<-temp$well
      s[[i]]$well<-temp$well
      s[[i]]<-get.num.AE(s[[i]])
      s[[i]]$rec.time<-temp$rec.time
      
    }#end of else for timepoint NE "00"
    
    #allb vars: beg  end  IBI len    durn  mean.isis SI
    #beg=index of first spike included in the burst in question
    #end=index of last spike included in the burst in question
    #len=# spikes in the burst
    #durn=time within a burst
    #mean.isis=mean interspike interval within a burst
    #(len-1)*mean.isis~=durn
    mi.par <- list(beg.isi =    0.1,
                   end.isi =    0.25,
                   min.ibi =    0.8,
                   min.durn =   0.05,
                   min.spikes = 5)
    
    s[[i]]$allb <- lapply(s[[i]]$spikes, mi.find.bursts)
    s[[i]]$bs<-calc.burst.summary(s[[i]])
    
  }#end of for loop that loads all data
  s
  
}
