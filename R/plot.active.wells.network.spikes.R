plot.active.wells.network.spikes <-
function(nspikes) {
	active.wells <- active.wells.network.spikes(nspikes)$ns.all
	if (length(active.wells)>0) {
		for (j in 1:length(active.wells)) {
			plot(active.wells[[j]], main = names(active.wells)[j],ylab='Count', xlab='Time (s)')
			y<- as.vector(active.wells[[j]]$mean)
			plot(ts(y,start = c(-(length(y)-1)/2,1)), xlab='Time (ms)', ylab='Count', main=paste('Mean NS for', 
				names(active.wells)[j],sep = " "))
		}
	}
}
