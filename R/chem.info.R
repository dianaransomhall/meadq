chem.info <-
function(s2,   
        masterChemFile="L:/Lab/NHEERL_MEA/Hall/NanoParticles/MaestroExperimentLog.csv" ) {
  #get chemical info
  #*********load chemical list*************
  # read in masterChemFile
  masterChem=read.csv(masterChemFile)
  masterCD<-as.data.frame(masterChem)
  #remove extraneous NA columns
  masterCD<-masterCD[,1:9]
  
  #reconstruct file names
  temp1<-paste(masterCD$Project, masterCD$Experiment.Date,masterCD$Plate.SN,sep="_")
  temp2<-paste('DIV',masterCD$DIV,sep="")
  temp3<-paste(temp1,temp2,sep="_")
  #add a new column called masterCD$filenames
  masterCD$filenames<-temp3
  
  #ensure log file is ordered correctly so it can be read in correctly
  masterCD<-masterCD[order(masterCD$Experiment.Date,masterCD$Plate.SN,masterCD$Well),]
  
  #****match wells to chemicals *****
  title<-strsplit(basename(s2$file),".h5")[[1]][1]
  fileName<-substring(title, 
                      1,nchar(title)-7)
  for (i in 1:length(masterCD$file)){
    if (fileName==masterCD$file[i]){
    for (j in 1:length(goodwells)){
        if (masterCD$Well[i]==s2$goodwells[j]) {
          s2$treatment[j]<-paste(masterCD$Treatment[i])
          s2$size[j]=paste(masterCD$Size[i])
          s2$dose[j]=paste(masterCD$Dose[i])
        }
      }
    }
  }
  s2
}
