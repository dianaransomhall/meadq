

PCA.by.well<-function(filename.data = NULL , trt.params.wanted = NULL, 
           output.folder = NULL , ctr.params.wanted = NULL , vars.wanted = NULL  ){ 
  
  #load necessary packages
  library(sjemea)
  library(FactoMineR)  
  
    
  #data file
  if ( is.null(filename.data) ){
    param.file<-choose.files(caption="Choose parameter file ")
    source( param.file, local=TRUE  ) 
  }

  print( paste("filename.data = ", filename.data, sep="") )
  print( paste("output.folder = ", output.folder, sep="") )
  print( paste("trt.params.wanted = ", trt.params.wanted, sep="") )
  print( paste("ctr.params.wanted = ", ctr.params.wanted, sep="") )
 
  if(!grepl( pattern=".csv", x = filename.data) ){
    warning("Error: filename.data not a .csv file")
  }
  stopifnot( grepl( pattern=".csv", x = filename.data)  )
  
  
  #set directories
  output.dir<-paste(output.folder, substring(date(),5,10), sep = "/")
  dir.create(output.dir )
  setwd(output.dir)  
  
  # read in file
  burstDataRaw<-read.csv(file = filename.data  )
  
  # get parameters needed
  if ( is.null( trt.params.wanted) ) {
    load( system.file("/data/chgv_parameters.rda"  , package='meadq' ) )
    
  }
  
  if ( !is.null(trt.params.wanted)&!is.null(ctr.params.wanted) ){
    trt.trt.wanted = trt.params.wanted$trt.wanted
    trt.DIV.wanted= trt.params.wanted$DIV.wanted
    trt.dose.wanted = trt.params.wanted$dose.wanted
    
    ctr.trt.wanted = ctr.params.wanted$trt.wanted
    ctr.DIV.wanted= ctr.params.wanted$DIV.wanted
    ctr.dose.wanted = ctr.params.wanted$dose.wanted
  } else {
    warning(caption= paste( "Error: ctr.params.wanted or trt.params.wanted is null",
            "ending PCA", sep="\n") )
    stopifnot( (!is.null(ctr.params.wanted)&!is.null(trt.params.wanted)) )
  }
 
  #### ERROR CATCHING
  # check that DIV.wanted and trt.wanted occur in data
  if ( !all(is.element( trt.trt.wanted, unique(burstDataRaw[,"trt"]) ) ) ){
    warning("1 or more of trt.wanted doesn't appear in filename.data")
    print("ending function")
  }
  stopifnot( all(is.element( trt.trt.wanted, unique(burstDataRaw[,"trt"]) ) ) )
  
  if ( !all(is.element( trt.DIV.wanted, unique(burstDataRaw[,"DIV"]) ) ) ){
    warning("1 or more of DIV.wanted doesn't appear in filename.data")
    print("ending function")
  }
  stopifnot( all(is.element( trt.DIV.wanted, unique(burstDataRaw[,"DIV"]) ) ) )
  
  if ( !all(is.element( trt.dose.wanted, unique(burstDataRaw[,"dose"]) ) ) ){
    warning("1 or more of dose.wanted doesn't appear in filename.data")
    print("ending function")
  }
  stopifnot( all(is.element( trt.dose.wanted, unique(burstDataRaw[,"dose"]) ) ) )
  
  ###  ERROR CHECKING CONTROL
  if ( !all(is.element( ctr.trt.wanted, unique(burstDataRaw[,"trt"]) ) ) ){
    warning("1 or more of trt.wanted doesn't appear in filename.data")
    print("ending function")
  }
  stopifnot( all(is.element( ctr.trt.wanted, unique(burstDataRaw[,"trt"]) ) ) )
  
  if ( !all(is.element( ctr.DIV.wanted, unique(burstDataRaw[,"DIV"]) ) ) ){
    warning("1 or more of DIV.wanted doesn't appear in filename.data")
    print("ending function")
  }
  stopifnot( all(is.element( ctr.DIV.wanted, unique(burstDataRaw[,"DIV"]) ) ) )
  
  if ( !all(is.element( ctr.dose.wanted, unique(burstDataRaw[,"dose"]) ) ) ){
    warning("1 or more of dose.wanted doesn't appear in filename.data")
    print("ending function")
  }
  stopifnot( all(is.element( ctr.dose.wanted, unique(burstDataRaw[,"dose"]) ) ) )
  
  trt.ind.wanted<-which(is.element( burstDataRaw[,"trt"], trt.trt.wanted)&
                      is.element(burstDataRaw[,"DIV"], trt.DIV.wanted )&
                        is.element(burstDataRaw[,"dose"], trt.dose.wanted ) )
  
  ctr.ind.wanted<-which(is.element( burstDataRaw[,"trt"], ctr.trt.wanted)&
                          is.element(burstDataRaw[,"DIV"], ctr.DIV.wanted )&
                          is.element(burstDataRaw[,"dose"],ctr.dose.wanted ) )
  
  sub.trt<-cbind( burstDataRaw[ trt.ind.wanted ,  ], 
                       grp=rep("trt", length(trt.ind.wanted) ) )
  sub.ctr<-cbind( burstDataRaw[ ctr.ind.wanted ,  ], 
                  grp= rep("ctr", length(ctr.ind.wanted)) )
  
  sub1<-as.data.frame( rbind(sub.trt,sub.ctr) )
  
  
  # make row names with chemical names
  temp.list = strsplit(x = as.character( sub1$file), split = "_", fixed = T)
  # mon.day = unlist( lapply( temp.list, function(x) substring(x[2],5,8) ) )
  row.names(sub1) = paste(substring(sub1$trt,1,min(length(sub1$trt),3) ) , 
            sub1$DIV,  sub1$well, seq(1,dim(sub1)[1] ), sep = "_" )
      
  #subset data to needed rows
  burstData<-sub1


#################Analysis

  numeric.cols.temp<-which( is.element(sapply(burstData, class), c("integer","numeric") ) )
  numeric.cols<-setdiff(names ( burstData[,numeric.cols.temp] ), c("Plate.SN", "date", "DIV") )

  if ( is.null(vars.wanted) ){
    vars.wanted = numeric.cols
    prefix="allNumeric"
    
  } else {
    prefix="subset"
    # error check
    if(!all( is.element(vars.wanted, numeric.cols) ) ){
      warning(caption=
        paste(paste("vars.wanted contains variables not present in ", filename.data, sep=''),
              "ending execution", sep="\n") )
    }
    stopifnot( all( is.element(vars.wanted,numeric.cols) )   )

  }


    # script meta data
    script = list();  script$R.vers = R.version.string ; script$data<-filename.data  
    script$name = "PCA.by.well.R"; 
    script$date = date()
    script$meta.text =  paste( paste(script$name, ",
                                     run on", script$date, sep=" " ),
                               paste("data set", script$data), sep ="\n") 

  # set options to display multiple windows
  options(device = "windows")

  #scale.unit=TRUE, means the variables need to be standardized
  #  use scale.unit = T if variables are in different units
  bs.pca <- PCA(burstData[,vars.wanted] ,  scale.unit=TRUE, ncp=5, graph=F )

  # % variance explained
  png(filename = paste(prefix, "Per_var_explained.png",sep="")  )
  plot(x=c(1:length(bs.pca$eig[,"percentage of variance"])), 
       y=bs.pca$eig[,"percentage of variance"],
       xlab="PC dimension", ylab="% Variance Exlained",
       main = "% Variance Explained by PC Dimension")
  dev.off()

  # get particular
  dimdesc(bs.pca)
  
  ###text files of pc dimensions       
  sink("dimdesc.txt")
  print(cat(script$meta.text) )
          
  print(dimdesc(bs.pca), append=T)
  sink()#removes the sink
  
          
  ###############PLOTS
  col.ind = rep(2, length(burstData[,1]) ); 
  # Negative is control
  ind.con<-which(burstData$grp=="ctr")
  col.ind[ind.con] = 1
  
  # individual plot
  # plot(bs.pca, choix="ind", new.plot=T, cex = .4, col.ind=col.ind ) #with names
  png( filename=paste(prefix, "individuals-dims-1-2.png",sep="") )
  plot(bs.pca, axes = c(1,2), choix="ind", new.plot=T, cex = 1.5, 
       label="none", col.ind=col.ind  )
  legend("topleft", c("ctr", "trt"), fill=c("black", "red"))
  mtext(paste(script$meta.text) , side=3, cex=.75)
  dev.off()
  
  # dim 2-3
  png( filename=paste(prefix, "individuals-dims-2-3.png",sep="") )
  plot(bs.pca, axes = c(2,3), choix="ind", new.plot=T, cex = 1.5, 
       label="none", col.ind=col.ind )
  legend("topleft", c("ctr", "trt"), fill=c("black", "red"))
  mtext(paste(script$meta.text) , side=3, cex=.75)
  dev.off()

  png( filename=paste(prefix, "individuals-dims-1-3.png",sep="") )
  plot(bs.pca, axes = c(1,3), choix="ind", new.plot=T, cex = 1.5, 
  label="none", col.ind=col.ind)
  legend("topleft", c("ctr", "trt"), fill=c("black", "red"))
  mtext(paste(script$meta.text) , side=3, cex=.75)
  dev.off()
  
  # Variable factors map
  png(filename=paste(prefix, "loadings-dims-1-2.png", sep="") )
  plot(bs.pca, choix="var", new.plot=T) 
  mtext(paste(script$meta.text) , side=3, cex=.75) 
  dev.off()
          
  #loadings dim 2 & 3
  png(filename=paste( prefix,"loadings-dims-2-3.png",sep="") )
  plot(bs.pca,choix="var",axes=2:3, new.plot=T)
  mtext(paste(script$meta.text) , side=3, cex=.75)
  dev.off() 
  
} # end of function








