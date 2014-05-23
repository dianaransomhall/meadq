plot.isis.by.plate <-
function(s) {
	isis.all <- unlist(s$isis)
	hist(isis.all,main = "Histogram of ISIs by Plate", xlab = "ISI length")
	hist(log10(isis.all),main = "Histogram of log(ISIs) by Plate", xlab = "log10(ISI length)")
}
