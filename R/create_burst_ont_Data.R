create_burst_ont_Data <-
function(h5Files,  save.rdata=F ){
  
  write.header=T
  plates = unique( sapply(strsplit(basename(h5Files), split="_"), function(x) x[3]) )
  num.plates = length( plates )
  
  for (cur.plate in 1:num.plates){
    
    #####  Read in and make data
    s=list()
    h5Files.wanted = h5Files[grep(x=h5Files,pattern= plates[cur.plate])]
    #order according to DIV
    temp.l<-sapply(sapply(strsplit(basename(h5Files.wanted),split="_"), 
                          function(x) x[4]) , nchar)
    temp.div= substring( 
      sapply( strsplit(basename(h5Files.wanted),split="_"), function(x) x[4]),
      rep(4,length(h5Files.wanted)),temp.l)
    
    h5Files.wanted = h5Files.wanted[order(as.numeric(temp.div)) ]
    
    for (cur.file in 1:length( h5Files.wanted ) ){
      
      s[[cur.file]]=h5.read.spikes( h5Files.wanted[cur.file] )
      # get meta-data
      g=c(); #remove any information stored in g
      g<-h5read(path.expand( h5Files.wanted[cur.file] ), name = "/")
      if ( is.element( "treatment", names(g) ) ){
        s[[cur.file]]$cw = substring(s[[cur.file]]$channel,1,2 )
        s[[cur.file]]$treatment = g$treatment         
        names(s[[cur.file]]$treatment) = g$well
        s[[cur.file]]$dose = g$dose
        names(s[[cur.file]]$dose) = g$well
        s[[cur.file]]$sCount = g$sCount
        names(s[[cur.file]]$sCount) = g$well
        s[[cur.file]]$units = g$units
        names(s[[cur.file]]$units) = g$well
        s[[cur.file]]$well = g$well
        s[[cur.file]]$summary.table = g$summary.table
        s[[cur.file]]$array = g$array     
        s[[cur.file]]$DIV = strsplit( basename(s[[cur.file]]$file), split="_")[[1]][4]
      } else{
        print ('Error: meta-data not available in .h5 file');
        
      }
      
      allb=list()
      allb <- lapply(s[[cur.file]]$spikes, mi.find.bursts)
      s[[cur.file]]$allb <- allb       
      s[[cur.file]]$bs<-calc.burst.summary( s[[cur.file]]  )
      
      h=calc.all.isi(s[[cur.file]], allb)
      s[[cur.file]]$isi = h
      
      
      nspikes.old <- calculate.network.spikes(s[[cur.file]])
      
      s[[cur.file]]$ns.all <- nspikes.old$ns.all
      
      nspikes <- summary.network.spikes.dh(s[[cur.file]],nspikes.old, ns.E = 2, sur)
      
      s[[cur.file]]$ns.all <- nspikes$ns.all
      
      
      # local correlation for one electrode with all other electrodes
      local.cor=c()
      local.cor<-local.corr.all.ont(s, t.p=cur.file )  
      s[[cur.file]]$local.cor<-local.cor
      
      # cv.time and network
      temp.list=c()
      temp.list<- cv.timestamps(s[[cur.file]]$spikes, s, cur.file)
      s[[cur.file]]$cv.time = temp.list[[1]]
      names(s[[cur.file]]$cv.time) = unique(s[[cur.file]]$cw)
      s[[cur.file]]$cv.network = temp.list[[2]]
      names(s[[cur.file]]$cv.network) = unique(s[[cur.file]]$cw)
      
      
      
      
      
      
      
      
      ####  AE Filtered data frame
      ##+++++++++++++++++  make data frame        
      ae.index.l<- split((s[[cur.file]]$meanfiringrate*60>=5), f=s[[cur.file]]$cw )
      ae.index.v<- unlist( split((s[[cur.file]]$meanfiringrate*60>=5), f=s[[cur.file]]$cw ) )
      # we need the wells that we have at least one AE
      well.names<-unique( subset(s[[cur.file]]$cw, ae.index.v) )
      num.wells = length(unique( subset(s[[cur.file]]$cw, ae.index.v) ) )
      well.indices = which(is.element(s[[cur.file]]$well, well.names ))
      
      df=c() #erase prior data
      if ( num.wells>0 ){
        
        df<-data.frame(
          date = rep( unlist(strsplit(basename(s[[cur.file]]$file), split="_"))[2]  , num.wells) , 
          Plate.SN = rep( strsplit(basename(s[[cur.file]]$file), split="_")[[1]][3], num.wells) ,
          DIV = rep( substring(strsplit(basename(s[[cur.file]]$file), split="_")[[1]][4],
                               4, nchar(strsplit(basename(s[[cur.file]]$file), split="_")[[1]][4])  ), num.wells) ,
          well = well.names ,
          trt = s[[cur.file]]$treatment[ well.names ]  , 
          dose = s[[cur.file]]$dose[ well.names ]  ,
          units = s[[cur.file]]$units[ well.names ]  ,
          meanfiringrate=sapply(by(subset(s[[cur.file]]$meanfiringrate, ae.index.v),
                                   subset(s[[cur.file]]$cw, ae.index.v), mean, na.rm=T), function(x) x[1]) ,
          
          burst.per.min = sapply( by( subset(s[[cur.file]]$bs$bursts.per.min, ae.index.v), 
                                      subset(s[[cur.file]]$cw, ae.index.v), mean, na.rm=T), function(x) x[1]) ,
          
          mean.isis = sapply(by(subset( s[[cur.file]]$bs$mean.isi, ae.index.v),
                                subset(s[[cur.file]]$cw, ae.index.v) , mean, na.rm=T), function(x) x[1]) ,
          
          per.spikes.in.burst =sapply(by(subset(s[[cur.file]]$bs$per.spikes.in.burst, ae.index.v),
                                         subset(s[[cur.file]]$cw, ae.index.v) , mean, na.rm=T), function(x) x[1]) ,
          
          mean.dur =sapply(by(subset( s[[cur.file]]$bs$mean.dur, ae.index.v),
                              subset( s[[cur.file]]$cw, ae.index.v) , mean, na.rm=T), function(x) x[1]) ,
          
          mean.IBIs =sapply(by(subset( s[[cur.file]]$bs$mean.IBIs, ae.index.v) ,
                               subset( s[[cur.file]]$cw, ae.index.v), mean, na.rm=T), function(x) x[1]) ,
          
          nAE = unlist( lapply( 
            by( s[[cur.file]]$meanfiringrate*60, s[[cur.file]]$cw, function(x) x>=5), sum ) )[well.names] ,
          
          nABE= unlist( lapply( 
            by( s[[cur.file]]$bs$bursts.per.min, s[[cur.file]]$cw, function(x) x>=0.5), sum ) )[well.names] ,
          
          ns.n = sapply(s[[cur.file]]$ns.all, 
                        function(x) x$brief['n'])[paste(well.names, "n", sep=".")] , #nspikes$ns.all$A8$brief
          ns.peak.m = sapply(s[[cur.file]]$ns.all , 
                             function(x) x$brief['peak.m'])[ paste(well.names, "peak.m", sep=".") ] ,
          ns.durn.m = sapply(s[[cur.file]]$ns.all, 
                             function(x) x$brief['durn.m'])[ paste(well.names, "durn.m", sep=".") ] ,
          ns.percent.of.spikes.in.ns = sapply(s[[cur.file]]$ns.all, 
                                              function(x) x$brief['percent.of.spikes.in.ns'])[ paste(well.names, "percent.of.spikes.in.ns", sep=".") ] ,
          ns.mean.insis = sapply(s[[cur.file]]$ns.all, 
                                 function(x) x$brief['mean.insis'])[ paste(well.names, "mean.insis", sep=".") ] ,
          ns.durn.sd = sapply(s[[cur.file]]$ns.all, 
                              function(x) x$brief['durn.sd'])[ paste(well.names, "durn.sd", sep=".") ] ,
          ns.mean.spikes.in.ns = sapply(s[[cur.file]]$ns.all, 
                                        function(x) x$brief['mean.spikes.in.ns'])[ paste(well.names, "mean.spikes.in.ns", sep=".") ] ,
          
          # not computed on active electrodes,problem?
          r=sapply(s[[cur.file]]$local.cor, mean, na.rm=T)[well.names] ,
          
          cv.time = s[[cur.file]]$cv.time[well.names]  ,
          cv.network = s[[cur.file]]$cv.network[well.names] ,
          
          file.name = rep( basename(s[[cur.file]]$file), num.wells )  
        ) #end data frame
        
        # write data to .csv file
        
        if (write.header ){
          write.table(  df, file = paste( paste( csv.filename.AEfilt, strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                                 plates[cur.plate],sep="_"), ".csv", sep="" ), 
                        sep=",", append = F, col.names=T, row.names=F )
          
        } else{
          write.table(  df, file = paste( paste( csv.filename.AEfilt, strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                                  plates[cur.plate],sep="_"), ".csv", sep="" ),
                        sep=",", append = T, col.names=F, row.names=F )
        }
        
        
      } # end of if num.wells>0
      
      
      
      
      
      
      
      
      
      #####  ABE Filtered Data
      # +++++++++++++make data frame
      ae.index.l<- split((s[[cur.file]]$meanfiringrate*60>=5), f=s[[cur.file]]$cw )
      ae.index.v<- unlist( split((s[[cur.file]]$meanfiringrate*60>=5), f=s[[cur.file]]$cw ) )
      
      abe.index<- unlist( split((s[[cur.file]]$bs$bursts.per.min>=0.5), f=s[[cur.file]]$cw ) )
      
      
      if ( sum(abe.index)>0 ){
        
        # we need the wells that we have at least one AE
        well.names<-unique( subset(s[[cur.file]]$cw, abe.index) )
        num.wells = length(unique( subset(s[[cur.file]]$cw, abe.index) ) )
        well.indices = which(is.element(s[[cur.file]]$well, well.names )) 
        
        #make data frame
        
        df2=c()
        df2<-data.frame(
          date = rep( unlist(strsplit(basename(s[[cur.file]]$file), split="_"))[2]  , num.wells) , 
          Plate.SN = rep( strsplit(basename(s[[cur.file]]$file), split="_")[[1]][3], num.wells) ,
          DIV = rep( substring(strsplit(basename(s[[cur.file]]$file), split="_")[[1]][4],
                               4, nchar(strsplit(basename(s[[cur.file]]$file), split="_")[[1]][4])  ), num.wells) ,
          well = well.names ,
          trt = s[[cur.file]]$treatment[well.names]  , 
          dose = s[[cur.file]]$dose[ well.names ]  ,
          units = s[[cur.file]]$units[ well.names ]  ,         
          meanfiringrate = sapply(by(subset(s[[cur.file]]$meanfiringrate, abe.index),
                                     subset(s[[cur.file]]$cw, abe.index), mean, na.rm=T), function(x) x[1]) ,
          
          burst.per.min = sapply( by( subset(s[[cur.file]]$bs$bursts.per.min, abe.index), 
                                      subset(s[[cur.file]]$cw, abe.index), mean, na.rm=T), function(x) x[1]) ,
          
          mean.isis = sapply(by(subset( s[[cur.file]]$bs$mean.isi, abe.index),
                                subset(s[[cur.file]]$cw, abe.index) , mean, na.rm=T), function(x) x[1]) ,
          
          per.spikes.in.burst =sapply(by(subset(s[[cur.file]]$bs$per.spikes.in.burst, abe.index),
                                         subset(s[[cur.file]]$cw, abe.index) , mean, na.rm=T), function(x) x[1]) ,
          
          mean.dur =sapply(by(subset( s[[cur.file]]$bs$mean.dur, abe.index),
                              subset( s[[cur.file]]$cw, abe.index) , mean, na.rm=T), function(x) x[1]) ,
          
          mean.IBIs =sapply(by(subset( s[[cur.file]]$bs$mean.IBIs, abe.index) ,
                               subset( s[[cur.file]]$cw, abe.index), mean, na.rm=T), function(x) x[1]) ,
          
          nAE = unlist( lapply( 
            by( s[[cur.file]]$meanfiringrate*60, s[[cur.file]]$cw, function(x) x>=5), sum ) )[well.names] ,
          
          nABE = unlist( lapply( 
            by( s[[cur.file]]$bs$bursts.per.min, s[[cur.file]]$cw, function(x) x>=0.5), sum ) )[well.names] ,
          
          ns.n = sapply(s[[cur.file]]$ns.all, 
                        function(x) x$brief['n'])[paste(well.names, "n", sep=".")] , #nspikes$ns.all$A8$brief
          ns.peak.m = sapply(s[[cur.file]]$ns.all , 
                             function(x) x$brief['peak.m'])[ paste(well.names, "peak.m", sep=".") ] ,
          ns.durn.m = sapply(s[[cur.file]]$ns.all, 
                             function(x) x$brief['durn.m'])[ paste(well.names, "durn.m", sep=".") ] ,
          ns.percent.of.spikes.in.ns = sapply(s[[cur.file]]$ns.all, 
                                              function(x) x$brief['percent.of.spikes.in.ns'])[ paste(well.names, "percent.of.spikes.in.ns", sep=".") ] ,
          ns.mean.insis = sapply(s[[cur.file]]$ns.all, 
                                 function(x) x$brief['mean.insi'])[ paste(well.names, "mean.insi", sep=".") ] ,
          ns.durn.sd = sapply(s[[cur.file]]$ns.all, 
                              function(x) x$brief['durn.sd'])[ paste(well.names, "durn.sd", sep=".") ] ,
          ns.mean.spikes.in.ns = sapply(s[[cur.file]]$ns.all, 
                                        function(x) x$brief['mean.spikes.in.ns'])[ paste(well.names, "mean.spikes.in.ns", sep=".") ] ,
          
          
          # not computed on active electrodes,problem?
          r=sapply(s[[cur.file]]$local.cor, mean, na.rm=T)[well.names] ,
          
          cv.time=s[[cur.file]]$cv.time[well.names]  ,
          cv.network=s[[cur.file]]$cv.network[well.names] ,
          
          file.name = rep( basename(s[[cur.file]]$file), num.wells )  
        )
        
        
        
        # write data to .csv file
        if ( write.header ){
          write.table(  df2, file= paste( paste( csv.filename.ABEfilt, strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                        plates[cur.plate],sep="_"), ".csv", sep="" ) ,
                         sep=",", append = F, col.names=T, row.names=F )
          
        } else{
          write.table(  df2, file= paste( paste( csv.filename.ABEfilt, strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                                                 plates[cur.plate],sep="_"), ".csv", sep="" ),
                        sep=",", append = T, col.names=F, row.names=F )
        }
        
        
        
        
      } #end of if (sum(ABEind>0))
      
      
      
      write.header=F
    } #end loop through h5Files in current plate
    
    
    if (save.rdata ){
      save(s, file = paste(h5.dir ,  paste("/s",
                                           strsplit(basename(s[[cur.file]]$file),split="_")[[1]][2] ,
                                           plates[cur.plate] , 
                                           paste('thru',s[[cur.file]]$DIV, ".Rdata", sep="") ,
                                           sep="_" ), sep="")    ) 
    }
    
    print(paste('-----------------       Done with file # ', cur.file, sep= "") )
    
  } # end of loop through plates
  
  
}
