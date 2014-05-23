write.plate.summary.for.bursts <-
function(s,outputdir) {
	masterSum<-get.burst.info.averaged.over.well(s)
	for (i in 1:length(s)){
		basename <- get.file.basename(s[[i]]$file)
		csvfile<-paste(outputdir,"/",basename,"_bursts.csv",sep="")

	  	##########data frame summarized over well
  		#get number of object in masterSum[[1]] list
  		tempdf<-c(); tempcolnames<-c()
  		for (j in 2:length(masterSum[[i]])){
    			tempc<-unlist(masterSum[[i]][j])
    			tempdf<-cbind(tempdf,tempc)
    			tempcolnames<-c(tempcolnames,names(masterSum[[i]][j]) )
    		}#end of loop through masterSum list objects
  
  		#need to switch around columns so first columns come first
		if (dim(tempdf)[2] > 20) { #for now
			if (dim(tempdf)[1] == 1) {
				df<-cbind(t(tempdf[,21:25]),t(tempdf[,1:20]))
			} else {
    				df<-cbind(tempdf[,21:25],tempdf[,1:20])
			}
    			colnames<-c(tempcolnames[21:25],tempcolnames[1:20])
    			colnames(df)<-colnames
		}

  		##################channel by channel burst summary

  		#meta data and misc
  		#get vector of which wells are active
  		wellindex<-which(is.element(names(s[[i]]$treatment), unique(s[[i]]$cw)) )
  		well<-c(); treatment<-c(); size<-c(); dose<-c(); file<-c();
  
  		file<-rep(strsplit( basename(s[[i]]$file),".h5")[[1]][1], length(s[[i]]$cw))
  		well<-s[[i]]$cw
  
  		#channel data frame
  		df2<-cbind(file,well,as.data.frame( s[[i]]$bs[1:length(s[[i]]$bs)]) )
  
 		
      	#write a title
    		write.table("Bursting Analysis averaged over Each Well",
                csvfile, sep=",", append=FALSE,row.names=FALSE,col.names=FALSE) 
  

      	write.table(paste("file= ",  strsplit( basename(s[[i]]$file),".h5")[[1]][1], sep=""),
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE) 
      	write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
      	#recording time
      	write.table(paste("recording time (s): [", paste(s[[i]]$rec.time[1],round(s[[i]]$rec.time[2]), sep=" ,"),
            "]",sep=""),csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  
      	write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  
      	#summary write data
		if (dim(df)[1] == 1) {
			suppressWarnings(write.table(t(df[,-c(1:3)]),
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
		} else {
			suppressWarnings(write.table(df[,-c(1:3)],
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))

		}
      	
      	#new lines
      	write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
      	write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
      
      	#title
      	write.table("Channel Burst Summary",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
      	write.table(paste("file= ",  strsplit( basename(s[[i]]$file),".h5")[[1]][1], sep=""),
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE) 
      	write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
      
      	#channel data
      	suppressWarnings(write.table(df2,
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
    		#new lines
    		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
    		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
	}#end of loop through writting tables
}
