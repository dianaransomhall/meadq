get.div <-
function(s) {
	div <- NA
	t1 <- strsplit(s$file,'_',fixed = TRUE)
	for (i in t1[[1]]) {
		if (nchar(i) > 2 && substr(i,1,3) == "DIV") {
			div <- as.numeric(substr(i,4,nchar(i)))
		}
	}
	div
}
