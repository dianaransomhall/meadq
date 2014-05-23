chem.info.2 <-
function(file,   
                      masterChemFile=masterChemFile) {
  
  #get chemical info
  #*********load chemical list*************
  # read in masterChemFile
  masterChem=read.csv(masterChemFile)
  masterCD<-as.data.frame(masterChem)
  #remove extraneous NA columns
  masterCD<-masterCD[,1:9]
  
  #reconstruct file names
  temp1<-paste(masterCD$Project, masterCD$Experiment.Date,masterCD$Plate.SN,sep="_")
  
  #add a new column called masterCD$filenames
  masterCD$filenames<-temp1
  
  #ensure log file is ordered correctly so it can be read in correctly
  masterCD<-masterCD[order(masterCD$Experiment.Date,masterCD$Plate.SN,masterCD$Well),]
  
  #****match wells to chemicals *****
  shortFileName<-paste( strsplit(basename(file),"_")[[1]][1],
                        strsplit(basename(file),"_")[[1]][2],
                        strsplit(basename(file),"_")[[1]][3],sep="_")
  
  plate.chem.info<-list()
  count=1;
  for (i in which(shortFileName==masterCD$filename) ){
    #get all info from chem list
    plate.chem.info$well[count]<-paste(masterCD$Well[i])
    plate.chem.info$treatment[count]<-paste(masterCD$Treatment[i])
    plate.chem.info$size[count]<-paste(masterCD$Size[i])
    plate.chem.info$dose[count]<-paste(masterCD$Dose[i])
    plate.chem.info$units[count]<-paste(masterCD$Units[i])
    count=count+1
    
  }#end of for loop through masterCD$file
  
  if (!is.element(length(plate.chem.info$well),c(12,48)) ){
    print(paste("Info exisits for ",length(plate.chem.info$well),
                " wells; Some wells have no data.", sep=""))
  }
  plate.chem.info
}
