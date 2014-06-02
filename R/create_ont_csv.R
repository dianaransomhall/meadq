# create_ont_csvFile.R
# Diana Hall
# 3-14-2014
# purpose: to create a burst data automatically from package



create_ont_csv<-function( h5Files = NULL , save.rdata = TRUE, param.file = NULL ){  
  
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
  
  
  # file names 
  assign( "csv.filename.AEfilt",paste( prepared.dir, "/ont_data_summary_AEfilt.csv",sep=""),
          envir = .GlobalEnv )
  assign( "csv.filename.ABEfilt",paste( prepared.dir, "/ont_data_summary_ABEfilt.csv",sep=""  ),
          envir = .GlobalEnv )
  
 
  if ( is.null( param.file ) ){
    data('chgv_parameters' )
  } else {
    source( param.file, local=TRUE  ) 
  }

  
  
  create_burst_ont_Data(h5Files=h5Files, save.rdata=save.rdata )

} # end of create_ont_csv.R


