plot.burst.base.1hr.sidebyside <-
function(s, resp, resp.label){
  
  #function needed to inter weave elements of a vector
  interleave <- function(v1,v2){
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }#end of interleave
  
  if (length(s[[1]]$well)<=12){
    well.layout=c(8,3)
    well.names <- paste(rep(LETTERS[3:1], each = 8), 
                        paste( rep(c(1,1,2,2,3,3,4,4) ,3), rep(c(0,1),12), sep="." ),sep = "")   
    
    names(well.names) <- paste( paste(rep(LETTERS[3:1], each = 8), 
                                      paste( rep(c(1,1,2,2,3,3,4,4) ,3), rep(c(0,1),12), sep="." ),sep = "")  ,
                                paste( c(interleave(s[[1]]$treatment[9:12], s[[2]]$treatment[9:12]),
                                         interleave(s[[1]]$treatment[5:8], s[[2]]$treatment[5:8]),
                                         interleave(s[[1]]$treatment[1:4], s[[2]]$treatment[1:4])),
                                       c(interleave(s[[1]]$size[9:12], s[[2]]$size[9:12]),
                                         interleave(s[[1]]$size[5:8], s[[2]]$size[5:8]),
                                         interleave(s[[1]]$size[1:4], s[[2]]$size[1:4])), sep=" ") ,
                                sep="=" )
    par.strip = list(cex = .6) 
    
    
  } else{
    well.layout=c(16,6)   
    well.names<-paste(rep(LETTERS[6:1],  each = 16), 
                      paste( rep(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8) ,6), rep(c(0,1),48), sep="." ),sep = "")
    names(well.names)<-cat( paste(  paste(rep(LETTERS[6:1],  each = 16), 
                                          paste( rep(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8) ,6), rep(c(0,1),48), sep="." ),sep=""),
                                    paste( c(interleave( s[[1]]$treatment[41:48], s[[2]]$treatment[41:48]),
                                             interleave( s[[1]]$treatment[33:40], s[[2]]$treatment[33:40]),
                                             interleave( s[[1]]$treatment[25:32], s[[2]]$treatment[25:32]),
                                             interleave( s[[1]]$treatment[17:24], s[[2]]$treatment[17:24]),
                                             interleave( s[[1]]$treatment[9:16], s[[2]]$treatment[9:16]),
                                             interleave( s[[1]]$treatment[1:8], s[[2]]$treatment[1:8]) ) ),
                                    paste( c(interleave( s[[1]]$dose[41:48], s[[2]]$dose[41:48]),
                                             interleave( s[[1]]$dose[33:40], s[[2]]$dose[33:40]),
                                             interleave( s[[1]]$dose[25:32], s[[2]]$dose[25:32]),
                                             interleave( s[[1]]$dose[17:24], s[[2]]$dose[17:24]),
                                             interleave( s[[1]]$dose[9:16], s[[2]]$dose[9:16]),
                                             interleave( s[[1]]$dose[1:8], s[[2]]$dose[1:8]) ),
                                           c(interleave( s[[1]]$units[41:48], s[[2]]$units[41:48]),
                                             interleave( s[[1]]$units[33:40], s[[2]]$units[33:40]),
                                             interleave( s[[1]]$units[25:32], s[[2]]$units[25:32]),
                                             interleave( s[[1]]$units[17:24], s[[2]]$units[17:24]),
                                             interleave( s[[1]]$units[9:16], s[[2]]$units[9:16]),
                                             interleave( s[[1]]$units[1:8], s[[2]]$units[1:8]) ),sep=""),
                                    
                                    
                                    sep = "\n") )
    
    par.strip = list(cex = .8) 
    
    if (FALSE){
      paste( c(interleave( s[[1]]$treatment[41:48], s[[2]]$treatment[41:48]),
               interleave( s[[1]]$treatment[33:40], s[[2]]$treatment[33:40]),
               interleave( s[[1]]$treatment[25:32], s[[2]]$treatment[25:32]),
               interleave( s[[1]]$treatment[17:24], s[[2]]$treatment[17:24]),
               interleave( s[[1]]$treatment[9:16], s[[2]]$treatment[9:16]),
               interleave( s[[1]]$treatment[1:8], s[[2]]$treatment[1:8]) ),
             c(interleave( s[[1]]$size[41:48], s[[2]]$size[41:48]),
               interleave( s[[1]]$size[33:40], s[[2]]$size[33:40]),
               interleave( s[[1]]$size[25:32], s[[2]]$size[25:32]),
               interleave( s[[1]]$size[17:24], s[[2]]$size[17:24]),
               interleave( s[[1]]$size[9:16], s[[2]]$size[9:16]),
               interleave( s[[1]]$size[1:8], s[[2]]$size[1:8]) ),sep=" ")
    }
    
    
  }
  
  active.wells.1<-paste( axion.elec2well(s[[1]]$channels),rep("0",length(s[[1]]$channels)),sep=".")
  active.wells.2<-paste( axion.elec2well(s[[2]]$channels),rep("1",length(s[[2]]$channels)),sep=".")
  
  
  if (length(strsplit(resp,"$",fixed=TRUE)[[1]])>1 ){
    temp1<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , get(strsplit(resp,"$",fixed=TRUE)[[1]][1], s[[1]]) )
    temp2<-get(strsplit(resp,"$",fixed=TRUE)[[1]][2] , get(strsplit(resp,"$",fixed=TRUE)[[1]][1], s[[2]]) )
    response<-c(temp1,temp2)
  } else {
    temp1<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s[[1]])
    temp2<-get(strsplit(resp,"$",fixed=TRUE)[[1]][1],  s[[2]])   
    response<-c(temp1,temp2)
  }
  
  
  listData<-list()
  
  listData$channels<-c(s[[1]]$channels, s[[2]]$channels )
  listData$active.wells<-c(active.wells.1,active.wells.2)
  
  
  
  p <- xyplot(response ~ factor(channels) | 
                factor(active.wells, labels=names(well.names),levels = well.names) ,
              data = listData , drop.unused.levels = FALSE, layout = well.layout, 
              xlab = "Channels within well",             
              ylab = paste(resp.label), pch = 20 , 
              main= paste( paste(resp.label, " by Channels within Wells",sep=""), 
                           paste("file= ",  strsplit( basename(s[[1]]$file),".h5")[[1]][1], sep=""), 
                           paste("Generated by burst_table_Plots.R on ",Sys.time(),sep=""), 
                           paste("written by Diana Hall using ",R.Version()$version.string,sep=""),                               
                           sep='\n'),
              scales = list(x = list(relation = "free",draw = FALSE)),
              #par.strip.text = par.strip 
              
              par.strip.text = list(cex=.75, lines=3, adj=.5)
  )
  
  print(p)
  p
  
}
