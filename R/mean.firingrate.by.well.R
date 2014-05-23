mean.firingrate.by.well <-
function(s) {
	df1 <- aggregate(s$meanfiringrate, by = list(s$cw),FUN = mean,na.rm = T)
	df2 <- aggregate(s$meanfiringrate, by = list(s$cw),FUN = sum,na.rm = T)

	df <- cbind(df1,df2[,2],get.div(s))
	names(df) <- c("well","meanfiringrate","meanfiringrate_per_well","div")
	rownames(df)<- t(df["well"])
	df
}
