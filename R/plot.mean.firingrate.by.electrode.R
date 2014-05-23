plot.mean.firingrate.by.electrode <-
function(s) {
	wells <- unique(s$cw)
	if (length(wells)>0) {
		for (well in wells) {
			active.electrodes <- which(s$cw == well)
			df <- list()
			for (i in active.electrodes) {
				df[[i]] <- cbind(s$rates$times,
				s$rates$rates[,i],
				names(s$nspikes)[i])
			}
			df = do.call("rbind",df)
			maxy <- max(df[,2])
			colnames(df) <- c("time","meanfiringrate","electrode")
			plateinfo <- plateinfo(s$layout$array)
			d1 <- expand.grid(col=1:plateinfo$n.elec.c,row=1:plateinfo$n.elec.r)
			all.electrodes <- sort(paste(well,"_", d1[,"row"],d1[,"col"],sep=""))
			layout.electrodes <- c(plateinfo$n.elec.r, plateinfo$n.elec.c)
			df <- data.frame(df)
			
			p1 <- xyplot(meanfiringrate ~ time | factor(electrode,levels=all.electrodes),
				 data = df, 
				 main = paste("Mean Firing Rate per Second for Well ", well, ". Maximum firing rate:",maxy," Hz", sep = "" ),
				layout = layout.electrodes,type = "h",
				scales=list(
          				x=list(draw = FALSE),
          				y=list(draw = FALSE)),    
				drop.unused.levels = FALSE)
			print(p1)
		}
		p1
	}
	
}
