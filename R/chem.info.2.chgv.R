#this function returns a list with the chemical names for corresponding
#date, plate number and wells found in "file"
chem.info.2.chgv<-function(file, masterChemFile=masterChemFile, debug=F) {
  #get chemical info
  #*********load chemical list*************
  # read in masterChemFile
  masterChem=read.csv(masterChemFile)
  raw.masterCD<-as.data.frame(masterChem)
  
  # Do a check to make sure all data is there
  actual.colnames<-colnames(raw.masterCD)
  good.colnames<-c("Treatment", "Plate.SN", "Experiment Date", "Dose","Project",
                   "Units", "Genotype", "Pup", "Trt.DIV","Well", "DIV" )
  match.array<-sapply(actual.colnames, grepl, x=good.colnames,ignore.case=T) 
  stopifnot( all( apply(match.array, 1, any) ) ) #look for at least one match
  #stop if each name doesn't have at least one match 
  
  temp.masterCD=c()
  #treatment 
  temp.masterCD$Treatment<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Treat", ignore.case=T ) ]
  #Plate.SN 
  temp.masterCD$Plate.SN<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Plate", ignore.case=T ) ]
  #Experiment Date
  temp.masterCD$Experiment.Date<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Experiment", ignore.case=T ) ]
  # Dose 
  temp.masterCD$Dose<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Dose", ignore.case=T ) ]
  #Project 
  temp.masterCD$Project<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Project", ignore.case=T ) ]
  #Units 
  temp.masterCD$Units<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Units", ignore.case=T ) ]
  #Genotype 
  temp.masterCD$Genotype<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Geno", ignore.case=T ) ]
  #Pup 
  temp.masterCD$Pup<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Pup", ignore.case=T ) ]
  #Trt.DIV
  temp.masterCD$Trt.DIV<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Trt.DIV", ignore.case=T ) ]
  #well
  temp.masterCD$Well<-
    raw.masterCD[ ,grepl(x=actual.colnames, pattern="Well", ignore.case=T ) ]
  #DIV
  temp.masterCD$DIV<-raw.masterCD[ ,"DIV" ]
  
  #reconstruct file names
  temp1<-paste(temp.masterCD$Project, temp.masterCD$Experiment.Date,
               temp.masterCD$Plate.SN,sep="_")
  
  #add a new column called masterCD$filenames
  temp.masterCD$filename<-temp1
  temp.masterCD<-as.data.frame(temp.masterCD)
  ##############################################################3
  if(debug){ print(temp.masterCD) }
  
  #ensure log file is ordered correctly so it can be read in correctly
  masterCD<-temp.masterCD[order(temp.masterCD$Experiment.Date,temp.masterCD$Plate.SN,
                                temp.masterCD$Well), ]
  ################################################################
  if(debug){ print(masterCD) }
  #****match wells to chemicals *****
  shortFileName<-paste( strsplit(basename(file),"_")[[1]][1],
                        strsplit(basename(file),"_")[[1]][2],
                        strsplit(basename(file),"_")[[1]][3],sep="_")
  
  plate.chem.info=list(treatment=c(),dose=c(), units=c(),
                       genotype=c(), pup=c(), trt.div=c(),well=c(),DIV=c())
  count=1;
  for (i in which(shortFileName==masterCD$filename) ){
    #get all info from chem list
    plate.chem.info$treatment[count]<-paste(masterCD$Treatment[i])
    plate.chem.info$dose[count]<-paste(masterCD$Dose[i])
    plate.chem.info$units[count]<-paste(masterCD$Units[i])
    plate.chem.info$genotype[count]<-paste(masterCD$Genotype[i])
    plate.chem.info$pup[count]<-paste(masterCD$Pup[i])
    plate.chem.info$trt.div[count]<-paste(masterCD$Trt.DIV[i])
    plate.chem.info$well[count]<-paste(masterCD$Well[i])
    count=count+1
    
  }#end of for loop through masterCD$file
  if(debug){ print(plate.chem.info) }
  
  if (!is.element(length(plate.chem.info$well),c(12,48)) ){
    print(paste("Info exisits for ",length(plate.chem.info$well),
                " wells; Some wells have no data.", sep=""))
  }
  plate.chem.info
}




