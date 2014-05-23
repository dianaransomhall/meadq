get.all.electrodes <-
function(r) {
	plate <- plateinfo(r$layout$array)
	wells <- as.matrix(sort(plate$wells))
	result <- as.vector(apply(wells,c(1,2),function(well) {get.electrode.layout(r,well)$electrodes}))
	result 
}
