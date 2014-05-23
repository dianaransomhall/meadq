calculate.isis <-
function(s) {
	
	s$isis <- lapply(s$spikes,diff)
	s$mean.isis <- lapply(s$isis,mean)
	s$sd.isis <- lapply(s$isis,sd)
	return(s)
}
