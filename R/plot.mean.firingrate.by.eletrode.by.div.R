plot.mean.firingrate.by.eletrode.by.div <-
function(s) {
	electrode.stats <- lapply(s, function(d){cbind(d$meanfiringrate,d$cw, get.div(d))})
	electrode.stats.all <- do.call("rbind",electrode.stats)
	electrode.names <- row.names(electrode.stats.all)
	electrode.stats.all <- suppressWarnings(data.frame(cbind(electrode.names, electrode.stats.all[,1:3])))
	names(electrode.stats.all) <- c("electrode","meanfiringrate","well","div")
	electrode.stats.all$div <- as.numeric(as.vector(electrode.stats.all$div))
	electrode.stats.all$meanfiringrate <- as.numeric(as.vector(electrode.stats.all$meanfiringrate))
	electrode.stats.all$electrode <- as.character(as.vector(electrode.stats.all$electrode))


	wells <- unique(electrode.stats.all$well)
	if (length(wells)>0) {
		for (active.well in wells) {
			df <- electrode.stats.all[which(electrode.stats.all$well == active.well),]
			layout.info <- get.electrode.layout(s[[1]],active.well)
			maxy <- max(df$meanfiringrate)

			p1 <- xyplot(meanfiringrate ~ div | factor(electrode,levels=layout.info$electrodes),
				 data = df, 
				 main = paste("Mean Firing Rate across DIV's for ", active.well, ". Maximum firing rate:",round(maxy,2)," Hz", sep = "" ),
				layout = layout.info$layout,
				#type = "h",
				#scales=list(
          			#	y=list(draw = FALSE)),    
				drop.unused.levels = FALSE)
			print(p1)
		}
	}
}
