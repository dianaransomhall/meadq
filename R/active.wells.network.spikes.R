active.wells.network.spikes <-
function(nspikes) {
	active.wells <- nspikes
	active.wells$ns.all <- nspikes$ns.all[sort(nspikes$wells[sapply(nspikes$wells,function(well) {
		!is.na(nspikes$ns.all[[well]]$brief[[1]]) & nspikes$ns.all[[well]]$brief[[1]] > 0})
	])]
	active.wells
}
