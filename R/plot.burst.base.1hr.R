plot.burst.base.1hr<-function(s , resp, resp.label, ctr.file.ind=1 , trt.file.ind = 2 ){  
  
  #resp="meanfiringrate" ; resp.label="Mean Firing Rate (Hz)"  
  if ( length(s[[ctr.file.ind]]$well)<=12 ){
    well.layout=c(4,3)
    well.names <- paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = "")
    if (s[[ctr.file.ind]]$size[1]!="NA") {
      treatment_label<-paste(c(s[[ctr.file.ind]]$treatment[9:12],s[[ctr.file.ind]]$treatment[5:8],
                               s[[ctr.file.ind]]$treatment[1:4]),
                            c(s[[ctr.file.ind]]$size[9:12],s[[ctr.file.ind]]$size[5:8],
                              s[[ctr.file.ind]]$size[1:4]),sep=" ")
    } else if (!is.null(s[[ctr.file.ind]]$dose[1]) ) {
      treatment_label<-paste(c(s[[ctr.file.ind]]$treatment[9:12],
                               s[[ctr.file.ind]]$treatment[5:8],
                               s[[ctr.file.ind]]$treatment[1:4]),
                             c(s[[ctr.file.ind]]$dose[9:12],
                               s[[ctr.file.ind]]$dose[5:8],
                               s[[ctr.file.ind]]$dose[1:4]),sep=" ")
    } else {
      treatment_label<-paste(c(s[[ctr.file.ind]]$treatment[9:12],
                               s[[ctr.file.ind]]$treatment[5:8],
                               s[[ctr.file.ind]]$treatment[1:4]))
    }
    
    names(well.names) <- paste( paste(rep(LETTERS[3:1], each = 4), rep(1:4, 3), sep = ""),
                                treatment_label,sep='=')
    par.strip = list(cex = 1) 
    
  } else{
    well.layout=c(8,6)   
    well.names<-paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = "")
    if ( !is.null(s[[ctr.file.ind]]$size) ) {
      treatment_label<-paste(c(s[[ctr.file.ind]]$treatment[41:48],s[[ctr.file.ind]]$treatment[33:40],
                               s[[ctr.file.ind]]$treatment[25:32],
                            s[[ctr.file.ind]]$treatment[17:24],s[[ctr.file.ind]]$treatment[9:16],
                            s[[ctr.file.ind]]$treatment[1:8]),
                          c(s[[ctr.file.ind]]$size[41:48],
                            s[[ctr.file.ind]]$size[33:40],s[[ctr.file.ind]]$size[25:32],
                            s[[ctr.file.ind]]$size[17:24],
                            s[[ctr.file.ind]]$size[9:16],s[[ctr.file.ind]]$size[1:8]),
                          sep=" ")
    } else if ( !is.null( s[[ctr.file.ind]]$dose[1] ) ) {
      treatment_label<-paste(c(s[[ctr.file.ind]]$treatment[41:48],
                               s[[ctr.file.ind]]$treatment[33:40],
                               s[[ctr.file.ind]]$treatment[25:32],
                               s[[ctr.file.ind]]$treatment[17:24],
                               s[[ctr.file.ind]]$treatment[9:16],
                               s[[ctr.file.ind]]$treatment[1:8] ),
                             c( s[[ctr.file.ind]]$dose[41:48],
                               s[[ctr.file.ind]]$dose[33:40],
                               s[[ctr.file.ind]]$dose[25:32],
                               s[[ctr.file.ind]]$dose[17:24],s[[ctr.file.ind]]$dose[9:16],
                                s[[ctr.file.ind]]$dose[1:8]) ,
                             sep=" ")
      
    } else {
      treatment_label<-paste(c(s[[ctr.file.ind]]$treatment[41:48],
                               s[[ctr.file.ind]]$treatment[33:40],
                               s[[ctr.file.ind]]$treatment[25:32],
                               s[[ctr.file.ind]]$treatment[17:24],
                               s[[ctr.file.ind]]$treatment[9:16],
                               s[[ctr.file.ind]]$treatment[1:8]))
      
    }
    names(well.names) <- paste( paste(rep(LETTERS[6:1], each = 8), rep(1:8, 6), sep = ""),
                                treatment_label,sep='=')
    par.strip = list(cex = .45) 
    
  }
  
  s12=list()
  
  temp1<-axion.elec2well(s[[ctr.file.ind]]$channels)
  temp2<-axion.elec2well(s[[trt.file.ind]]$channels)
  s12$active.wells<-c(temp1,temp2)
  
  if (length(strsplit(resp,"$",fixed=TRUE)[[ctr.file.ind]])>1 ){
    response1<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , get(strsplit(resp,"$",fixed=TRUE)[[1]][1], 
                           s[[ctr.file.ind]]) )
  } else {
    response1<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s[[ctr.file.ind]])
  }
  
  if (length(strsplit(resp,"$",fixed=TRUE)[[1]])>1 ){
    response2<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , 
                   get(strsplit(resp,"$",fixed=TRUE)[[1]][1], s[[trt.file.ind]]) )
  } else {
    response2<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s[[trt.file.ind]])
  }
  
  s12$response<-c(response1, response2)
  
  s12$channels<-c(s[[ ctr.file.ind ]]$channels, s[[trt.file.ind]]$channels)
  
  if (!is.null(s[[ctr.file.ind]]$timepoint)){
    s12$timepoint<-c(rep(s[[ctr.file.ind]]$timepoint,length( response1 ) ), 
                     rep(s[[trt.file.ind]]$timepoint, length(response2) ) )
  } else{
    s12$timepoint<-c(rep("baseline",length( response1 ) ),
                     rep("comparisson ", length(response2) ) )
  }

  
  s12$file<-s[[ctr.file.ind]]$file
  
  #burstPlotPath = paste(burst.dir,"/all-h5Files-Plots.pdf",sep=""); pdf(file=burstPlotPath) 
  
  p<-xyplot(response ~ factor(channels) | 
              factor(active.wells, labels=names(well.names),levels = well.names),
            data = s12, group=timepoint, 
            drop.unused.levels = FALSE, layout = well.layout, 
            key= list(text=list(c("Timepoint","0","1")),
                      points=list(pch=c(NA, 1,2), col=c(NA,"red","blue") ),
                      space="right"),   #this adds a complete key
            xlab = "Channels within well",
            ylab = paste(resp.label,sep=""), 
            
            pch = c(1,2) , col=c("red", "blue"),
            main= paste( paste(resp.label, " by Channels within Wells",sep=""), 
                         paste("file= ",  strsplit( basename(s12$file),".h5")[[1]][1], sep=""), 
                         paste("Generated by burst_table_Plots.R on ",Sys.time(),sep=""), 
                         paste("written by Diana Hall using ",R.Version()$version.string,sep=""),                               
                         sep='\n') , 
            
            cex.main=1,  
            scales = list(x = list(relation = "free",
                                   draw = FALSE)),
            par.strip.text = par.strip,
            par.settings = list(layout.heights=list(strip=2)))
  
  
  print( p)
  p
}
