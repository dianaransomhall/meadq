get.data <-
function(){
  #load necessary packages
  library(sjemea)
  library(rhdf5)
  library(lattice)
  
  #get the directory containing the .h5 files
  h5Files<-sort(choose.files(caption="Choose .h5 Files or choose .Rdata file in h5Files folder") )
  
  root.dir<-dirname( dirname(h5Files) )
  #create directory for bursting
  suppressWarnings(dir.create(paste(root.dir,'/Analysis',sep='')))
  output.dir<-paste(root.dir,'/Analysis',sep='')
  return(list(h5Files = h5Files,output.dir = output.dir))	
}
