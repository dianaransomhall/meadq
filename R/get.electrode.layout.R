get.electrode.layout <-
function(r,well) {
	plateinfo <- plateinfo(r$layout$array)
	d1 <- expand.grid(col=1:plateinfo$n.elec.c,row=1:plateinfo$n.elec.r)
	electrodes <- sort(paste(well,"_", d1[,"row"],d1[,"col"],sep=""))
	layout <- c(plateinfo$n.elec.r, plateinfo$n.elec.c)
	return(list(electrodes  = electrodes, layout = layout))
}
