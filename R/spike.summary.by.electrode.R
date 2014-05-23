spike.summary.by.electrode <-
function(s) {
	s <- calculate.isis(s)
	electrodes <- get.all.electrodes(s)
	sum <- matrix(data = NA, nrow = length(electrodes), ncol = 4)
	colnames(sum) <- c("nspikes","meanfiringrate","meanisis","sdisis")
	rownames(sum) <- electrodes

	df <- cbind(s$nspikes,s$meanfiringrate,s$mean.isis,s$sd.isis)
	active.electrodes<-rownames(df)
	for (i in active.electrodes) {
		sum[i,] <- unlist(df[i,])
	}
	sum
}
