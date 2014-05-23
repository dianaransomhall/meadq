write.plate.summary.for.spikes <-
function(s,outputdir) {
	for (i in 1:length(s)) {
		basename <- get.file.basename(s[[i]]$file)
		csvfile <- paste(outputdir,"/",basename,"_spikes.csv",sep="")
		df <- spike.summary.by.electrode(s[[i]])
		df2 <- spike.summary.by.well(s[[i]])
		
		#recording time
		write.table(paste("recording time (s): [", paste(s[[i]]$rec.time[1],round(s[[i]]$rec.time[2]), sep=" ,"),
            	"]",sep=""),csvfile, sep=",", append=FALSE,row.names=FALSE,col.names=FALSE)
		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)
		write.table("Spike statistics for wells",	csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE) 
		df2 = cbind(rownames(df2),df2)
		suppressWarnings(write.table(df2,
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
		write.table(" ",csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE)

		write.table("Spike statistics for electrodes",
      		csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=FALSE) 		
  		df = cbind(rownames(df),df)
		colnames(df)[1] <- "electrode"
      	#summary write data
      	suppressWarnings(write.table(df,
                  csvfile, sep=",", append=TRUE,row.names=FALSE,col.names=TRUE))
	}
}
