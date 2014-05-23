# create_ont_csvFile.R
# Diana Hall
# 3-14-2014
# purpose: to create a burst data automatically from package



create_ont_csv<-function( h5Files = NULL , save.rdata = TRUE ){  
  
  #load necessary packages
  library(sjemea)
  library(rhdf5)
  library(lattice)
  
  #get the directory containing the .h5 files
  if (is.null(h5Files)){
    h5Files<-sort(choose.files(caption="Choose .h5 Files") )    
  }

  h5.dir<-dirname(h5Files[1])
  
  #set directories 
  root.dir<-dirname( dirname(h5Files[1]) )
  
  #create directory for burst Data
  prepared.dir<-paste( dirname(h5.dir[1]) , "prepared_data", sep="/")
  dir.create( prepared.dir )
  
  
  # csv.filename<-"F:/Duke/Angelman_20140211/prepared_data/ont_data_summary_AEfilt.csv"
  csv.filename.AEfilt<-paste( prepared.dir, "/ont_data_summary_AEfilt.csv",sep=""  )
  csv.filename.ABEfilt<-paste( prepared.dir, "/ont_data_summary_ABEfilt.csv",sep=""  )
  
 
  param.file<-choose.files(caption="Choose parameter file ")
  source( param.file  ) 
  
  
  create_burst_ont_Data(h5Files=h5Files, save.rdata=save.rdata )

} # end of create_ont_csv.R


