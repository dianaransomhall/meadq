calculate.network.spikes <-
function(e,sur=100) {
	#get well information
	plateinfo <- plateinfo(e$layout$array)
	wells <- plateinfo$wells
	names(wells) <- wells #keep the names valid.
	wells.layout <- plateinfo$layout
	##lets just use lapply
	ns.all<-lapply(wells, function(well) {
      	compute.ns(e, ns.T = ns.T, ns.N = ns.N, sur=sur, whichcells = well)})
	return(list(wells = wells,ns.all = ns.all,wells.layout = wells.layout))
}
