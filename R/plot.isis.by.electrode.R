plot.isis.by.electrode <-
function(s) {
	wells <- unique(s$cw)
	if (length(wells)>0) {
		for (well in wells) {
			active.electrodes <- which(s$cw == well & as.vector(unlist(lapply(s$isis,length))) >0)
			df <- list()
			for (i in 1:length(active.electrodes)) {
				df[[i]] <- cbind(s$isis[[active.electrodes[i]]],
				names(s$isis)[active.electrodes[i]])
			}
			df = do.call("rbind",df)
			colnames(df) <- c("isis","electrode")
			plateinfo <- plateinfo(s$layout$array)
			d1 <- expand.grid(col=1:plateinfo$n.elec.c,row=1:plateinfo$n.elec.r)
			all.electrodes <- sort(paste(well,"_", d1[,"row"],d1[,"col"],sep=""))
			layout.electrodes <- c(plateinfo$n.elec.r, plateinfo$n.elec.c)
			df <- data.frame(df)
			df$isis <- as.numeric(as.vector(df$isis))
			df$electrode <- as.character(as.vector(df$electrode))


			p1 <- histogram(~ isis | factor(electrode,levels=all.electrodes),
				 data = df, breaks = 10,
				 main = paste("ISIs histogram plot for ", well, sep = "" ),
				layout = layout.electrodes,
				drop.unused.levels = FALSE)
			print(p1)
			
			p2 <- histogram(~ log(isis) | factor(electrode,levels=all.electrodes),
				 data = df, breaks = 10,
				 main = paste("log(ISIs) histogram plot for ", well, sep = "" ),
				layout = layout.electrodes,
				drop.unused.levels = FALSE)
			print(p2)

		}
		p2
	}
	
}
