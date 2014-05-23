get.project.plate.name <-
function(file) {
	tempname<-strsplit( basename(file),".h5")[[1]][1]
	basename<-paste( strsplit(tempname, "_")[[1]][1],strsplit(tempname, "_")[[1]][2],
                     strsplit(tempname, "_")[[1]][3],sep="_")
	basename
}
