spike.summary.by.well <-
function(s) {
	plate <- plateinfo(s$layout$array)
	wells <- sort(plate$wells)
	s$isis <- lapply(s$spikes,diff)
	sum <- matrix(data = NA, nrow = length(wells), ncol = 8)
	colnames(sum)<- c("nAE","nspikes_by_well","meanfiringrate_by_well","meanfiringrate_by_all_ectctordes","meanfiringrate_by_active_electordes","sdfiringrate_by_active_electordes","meanisis","sdisis")
	rownames(sum) <- wells
	nelectrodes <- plate$n.elec.r *plate$n.elec.c
	if (!is.null(s$goodwells)) {
		for (j in 1:length(s$goodwells)){
			icurrentwell<-(s$goodwells[j]==s$cw)
			incurrentwell<-which((s$goodwells[j]==s$cw))
			sum[s$goodwells[j],1] <- length(incurrentwell)
			sum[s$goodwells[j],2] <- sum(s$nspikes[icurrentwell])
			sum[s$goodwells[j],3] <- sum(s$meanfiringrate[icurrentwell])
			sum[s$goodwells[j],4] <- sum[s$goodwells[j],3] / nelectrodes 

			sum[s$goodwells[j],5] <- mean(s$meanfiringrate[icurrentwell])
			sum[s$goodwells[j],6] <- sd(s$meanfiringrate[icurrentwell])

			isis.all <- unlist(s$isis[icurrentwell])
			sum[s$goodwells[j],7] <- mean(isis.all)
			sum[s$goodwells[j],8] <- sd(isis.all)
		}
	}
	sum
}
