plot.plate.summary.for.bursts <-
function(s,outputdir) {
	for (i in (1:length(s))){
		basename <- get.file.basename(s[[i]]$file)
		burstPlotPath = paste(outputdir,"/",basename,"_burst_plot.pdf",sep="")
		pdf(file=burstPlotPath) 

		#layout 
  		p<-plot.mealayout.1(s[[i]]$layout, use.names=T, cex=0.25)
  		title(main= paste( paste("Electrode Layout"), 
                     paste("file= ",  strsplit( basename(s[[i]]$file),".h5")[[1]][1], sep=""),                             
                     sep='\n'))

		#MFR
		p<- plot.meanfiringrate(s[[i]], main = "Mean Firing Rate by Plate (Hz)")
		#p<- plot(s[[i]], main = "", label.cells = FALSE, use.names = FALSE)

  		p<-channel.plot.by.well(s[[i]],resp="meanfiringrate", resp.label="Mean Firing Rate (Hz)")
		#Mean Duration
  		p<-channel.plot.by.well(s[[i]],resp="bs$mean.dur",resp.label="Mean Duration of Burst (s)")
		#plot of Number of bursts by channel and well  
  		p<-channel.plot.by.well(s[[i]],resp="bs$nbursts",resp.label="Number of Bursts")
		#mean Inter Burst Interval 
  		p<-channel.plot.by.well(s[[i]],resp="bs$mean.IBIs",resp.label="Mean IBIs (ms)")
		# mean ISI within bursts 
  		p<-channel.plot.by.well(s[[i]],resp="bs$mean.isis",resp.label="Mean ISI w/i Bursts (s)")
		#mean burst per minute 
  		p<-channel.plot.by.well(s[[i]],resp="bs$bursts.per.min",resp.label="Mean Burst per Minute")
		#mean spikes in a burst
  		p<-channel.plot.by.well(s[[i]],resp="bs$mean.spikes",resp.label="Mean # Spikes/Burst")
		#% spikes in a burst
  		p<-channel.plot.by.well(s[[i]],resp="bs$per.spikes.in.burst",resp.label="% Spikes/Burst")
		dev.off()  
	}
}
