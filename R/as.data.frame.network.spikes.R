as.data.frame.network.spikes <-
function(nspikes) {
	#useful function
	mean.ns.to.xy <- function(ns, well) {
  		## Convert the mean network spike into a dataframe suitable for lattice
  		## graphics.
  		if (!is.null(ns$mean)) {
    			m = ns$mean
    			t = as.vector(time(m))
    			d <- data.frame(t = t, y = as.vector(m), well = well)
    			d
  		}
	}
	
	#get data in right format for lattice
	xys <- mapply(SIMPLIFY = FALSE, function(ns, well) {
  		mean.ns.to.xy(ns, well)
	}, nspikes$ns.all, names(nspikes$ns.all))

	d <- do.call("rbind", xys) #merge all the data frames.
	d	
}
