#make.axion.map.to.h5.dh.R
#purpose: creating .h5 files that have well data in them
#Diana Hall
#06/02/2014


make.axion.map.to.h5.dh<-function(  ){    
  
  #load required packages
  library(rhdf5)
  
  #######start of data input########
  ######################################## 
  
  #get basename of files
  tempfiles<-sort(choose.files(caption="Select .mapTimestamp Files"))
  mapTimestampFiles=sort(tempfiles) 
  
  
  #determine directory for R code and load function bank 
  root.dir<-dirname(dirname(mapTimestampFiles[1]))
  #axion.map.to.h5 needs data.dir=directory of .mapTimestamps
  data.dir<-dirname(mapTimestampFiles[1])
  #directory of h5 files
  dir.create(paste(dirname(data.dir),'/h5Files',sep='') )
  # <<- assigns global variables
  h5.dir<<-paste(dirname(data.dir),'/h5Files',sep='')
  
  #get master chemical list
  masterChemFile<-choose.files(caption="Select Master Chemical File")
  
  setwd(data.dir)
  
  L=length(mapTimestampFiles)
  
  #convert .mapTimestamps to .h5 files
  for (i in 1:L){
    #load data from all files in folder (listed above)
    title<-strsplit(basename(mapTimestampFiles[i]), ".", fixed = TRUE)[[1]][1]
    #get plate chemical info for each file in the list
    plate.chem.info<-chem.info.2(mapTimestampFiles[i],masterChemFile)
    # read.table(masterChemFile, sep=",")
    #make h5 files that contain chemical info 
    axion.map.to.h5.dh(title,plate.chem.info) 
    
  }
  
  
  #######check h5file if so desired
  #h5file<-paste(h5.dir,"/",title,".h5",sep="")
  #data<-h5.read.spikes.dh(h5file)


}


