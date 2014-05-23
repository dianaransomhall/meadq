plot.mean.firingrate.by.well.by.div <-
function(s) {
	well.stats <- lapply(s, function(d){d$well.stats})
	well.stats.all <- do.call("rbind",well.stats)
	plateinfo <- plateinfo(s[[1]]$layout$array)
	wells <- plateinfo$wells
	names(wells) <- wells #keep the names valid.
	wells.layout <- plateinfo$layout

	p1 <- xyplot(meanfiringrate ~ div | factor(well,levels=wells), data = well.stats.all, 
		main = "Mean Firing Rate across DIV's (Hz/electrode)",layout = wells.layout,
		drop.unused.levels = FALSE)
	print(p1)
	p2 <- xyplot(meanfiringrate_per_well ~ div | factor(well,levels=wells), data = well.stats.all, 
		main = "Mean Firing Rate across DIV's (Hz/well)",layout = wells.layout,
		drop.unused.levels = FALSE)
	print(p2)
	#return(list(p1=p1,p2=p2))
}
