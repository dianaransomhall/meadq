xyplot.network.spikes <-
function(nspikes) {
	p1 <- NULL
	## Produce the title for each well; note difference between '0' for zero
	## network spikes found in a well vs 'NA' (no spikes on that well).
	strip.names <- sapply(nspikes$wells, function(well) {
		n = nspikes$ns.all[[well]]
  		n.ns = n$brief[[1]]
  		paste(well, n.ns)
	})
	df <- as.data.frame.network.spikes(nspikes)
	if (!is.null(df)) {
		p1 <- xyplot(y ~ 10*t | factor(well, levels = nspikes$wells), data = df, strip = strip.custom(factor.levels = strip.names),
      		main = "Mean NS",xlab = "Time (ms)", ylab = "# electrodes", type = "l", drop.unused.levels = FALSE,
       		layout = nspikes$wells.layout)
		print(p1)
	p1
	}
}
