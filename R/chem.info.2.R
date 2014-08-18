chem.info.2<-function (file, masterChemFile = masterChemFile) 
{
  masterChem = read.csv(masterChemFile)
  masterCD.temp <- as.data.frame(masterChem)
  masterCD<-c()
  
  masterCD$Experiment.Date<-masterCD.temp[,
              grepl(pattern="xperiment{1}", 
                   names(masterCD.temp), 
                   perl=T)]
  
  masterCD$Plate.SN<-masterCD.temp[,
                                   grepl(pattern="[Pp]+late{1}", 
                                         names(masterCD.temp), 
                                         perl=T)]
  
  masterCD$Project<-masterCD.temp[,
                                  grepl(pattern="[Pp]roject{1}", 
                                        names(masterCD.temp), 
                                        perl=T)]
  
  if (sum(grepl(pattern="[Ww]ell{1}", names(masterCD.temp), perl=T))>=1 ){
    masterCD$Well<-masterCD.temp[,
      grepl(pattern="[Ww]ell{1}", 
            names(masterCD.temp), 
            perl=T)]
  }
  
  if (sum(grepl(pattern="[Dd]ose{1}", names(masterCD.temp), perl=T))>=1 ){
    masterCD$Dose<-masterCD.temp[,
      grepl(pattern="[Dd]ose{1}", 
            names(masterCD.temp), 
            perl=T)]
  }
  
  if (sum(grepl(pattern="[Tt]reat{1}", names(masterCD.temp), perl=T))>=1 ){
    masterCD$Treatment<-masterCD.temp[,
                                      grepl(pattern="[Tt]reat{1}", 
                                            names(masterCD.temp), 
                                            perl=T)]
  }
  
  if (sum(grepl(pattern="[Ss]ize{1}", names(masterCD.temp), perl=T))>=1 ){
    masterCD$Size<-masterCD.temp[,
                                 grepl(pattern="[Ss]ize{1}", 
                                       names(masterCD.temp), 
                                       perl=T)]
  }
  
  if (sum(grepl(pattern="[Uu]nit{1}", names(masterCD.temp), perl=T))>=1 ){
    masterCD$Units<-masterCD.temp[,
                                  grepl(pattern="[Uu]nit{1}", 
                                        names(masterCD.temp), 
                                        perl=T)]
  }
  
  
  temp1 <- paste(masterCD$Project, masterCD$Experiment.Date, 
                 masterCD$Plate.SN, sep = "_")
  masterCD$filenames <- temp1
  masterCD<-as.data.frame(masterCD)
  masterCD <- masterCD[ order(masterCD$Experiment.Date, 
                             masterCD$Plate.SN, 
                             masterCD$Well ), ]
  shortFileName <- paste(strsplit(basename(file), "_")[[1]][1], 
                         strsplit(basename(file), "_")[[1]][2], 
                         strsplit(basename(file),  "_")[[1]][3], 
                         sep = "_")
  plate.chem.info <- list()
  count = 1
  for (i in which(shortFileName == masterCD$filename)) {
    
    
    if (sum(grepl(pattern="[Ww]ell{1}", names(masterCD),perl=T))>=1 ){
      plate.chem.info$well[count] <- paste(masterCD$Well[i])
    }
    
    if (sum(grepl(pattern="[Tt]reat{1}", names(masterCD), perl=T))>=1 ){
      plate.chem.info$treatment[count] <- paste(masterCD$Treatment[i])
    }
    
    if (sum(grepl(pattern="[Dd]ose{1}", names(masterCD), perl=T))>=1 ){
      plate.chem.info$dose[count] <- paste(masterCD$Dose[i])
    }
    
    if (sum(grepl(pattern="[Ss]ize{1}", names(masterCD), perl=T))>=1 ){
      plate.chem.info$size[count] <- paste(masterCD$Size[i])
    }
    
    if (sum(grepl(pattern="[Uu]nit{1}", names(masterCD), perl=T))>=1 ){
      plate.chem.info$units[count] <- paste(masterCD$Units[i])
    }
    
    count = count + 1
  }
  
  if (!is.element(length(plate.chem.info$well), c(12, 48))) {
    print(paste("Info exisits for ", length(plate.chem.info$well), 
                " wells; Some wells have no data.", sep = ""))
  }
  plate.chem.info
}