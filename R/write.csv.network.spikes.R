write.csv.network.spikes <-
function(s,nspikes,outputdir) {
	active.wells <- active.wells.network.spikes(nspikes)$ns.all
	if (length(active.wells) >0) {
		newcol <-2 
		#2 to peak.min and peak.max
		p <- length(active.wells[[1]]$brief) + length(active.wells[[1]]$mean) + newcol
		nsdata <- matrix(0,length(active.wells),p)
		for (j in 1:length(active.wells)) {
			temp <- active.wells[[j]]
			nsdata[j,1:length(temp$brief)] <- temp$brief
			nsdata[j,length(temp$brief)+1] <- min(temp$measures[,"peak.val"])
			nsdata[j,length(temp$brief)+2] <- max(temp$measures[,"peak.val"])
			nsdata[j,(length(temp$brief)+newcol+1):p] <- as.double(temp$mean)
		}
		nsdata <- data.frame(nsdata)	
		names(nsdata)[1:length(temp$brief)] <- names(temp$brief)
		names(nsdata)[(length(temp$brief)+1):(length(temp$brief)+2)] <- c("peak.min","peak.max")

		for (j in 1:(p-length(temp$brief)-newcol)) {
			names(nsdata)[j+newcol+length(temp$brief)] = paste("t",j,sep = '')
		}
		nsdata<- cbind(names(active.wells),nsdata)
		names(nsdata)[1] <- "well"
		
		basename <- get.file.basename(s$file)
		csvfile= paste(outputdir,"/",basename,"_ns.csv",sep="")

      	write.table(paste("file= ",  strsplit( basename(s$file),".h5")[[1]][1], sep=""),
                  csvfile, sep=",", append=FALSE,row.names=FALSE,col.names=FALSE) 
      	write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
      	#recording time
      	write.table(paste("recording time (s): [", paste(s$rec.time[1],round(s$rec.time[2]), sep=" ,"),
            	"]",sep=""),csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
  
		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
	
		write.table("Network Spike analysis at well level",
                csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)   	
		suppressWarnings(write.table(nsdata,
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))

		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
		write.table("Network Spike analysis at electrode level",
                csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)  

 	
		en.df <- do.call("rbind",lapply(nspikes$ns.all, function(well) {
			temp <- well$en.brief
			temp
		}))
		en.df <- en.df[order(rownames(en.df)), ] 
		en.df <- cbind(rownames(en.df),en.df)
		colnames(en.df)[1] <- "electrode"
		suppressWarnings(write.table(en.df,
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))

	}
}
