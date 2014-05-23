plot.plate.summary.for.spikes <-
function(s,outputdir) {
	for (i in 1:length(s)) {
		basename <- get.file.basename(s[[i]]$file)
		spikePlotPath = paste(outputdir,"/",basename,"_spike_plot.pdf",sep="")
		pdf(file=spikePlotPath) 

		#layout 
  		p<-plot.mealayout.1(s[[i]]$layout, use.names=T, cex=0.25)
  		title(main= paste( paste("Electrode Layout"), 
                     paste("file= ",  strsplit(basename(s[[i]]$file),".h5")[[1]][1], sep=""),sep='\n'))
		#MFR
		p<- plot.meanfiringrate(s[[i]], main = "Mean Firing Rate by Plate (Hz)")
		#p<- plot(s[[i]],main = "Raster plots by channel", label.cells = FALSE, use.names = FALSE)
		p<- plot.isis.by.plate(s[[i]])
  		p<-channel.plot.by.well(s[[i]],resp="meanfiringrate", resp.label="Mean Firing Rate (Hz)")  
		p<-plot.mean.firingrate.by.electrode(s[[i]])
		p<-plot.isis.by.electrode(s[[i]])
		dev.off()
	}
}
