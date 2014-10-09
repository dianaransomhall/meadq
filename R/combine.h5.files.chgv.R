
#purpose: to combine .h5 files such as file1_001.h5, file2_002.h5, file3_003.h5
#input: h5Files to be combined
#output: list of combined files
combine.h5.files.chgv<-function(h5Files ){
  #load meta data
  s=list()
  h5Files<-sort(h5Files)
  for (i in 1:length(h5Files)){
    s[[i]]<-h5.read.spikes.chgv(h5Files[[i]])
    summary(s[[i]])
  }#end of reaching in spikes
  
  #filter spikes 
  s_combo<-list()
  cc=0#combo count, indexes the list after files have been combined
  for (i in 1:length(h5Files)){
    #is current file=baseline file?
    #need to add in how to deal with baseline file
    #being split into many files "001","002",.. etc
    if (substr(basename(s[[i]]$file),
               nchar(basename(s[[i]]$file))-5,
               nchar(basename(s[[i]]$file))-3) =="001" ){
      #increment combo_count
      cc=cc+1
      #s_combo has first of files
      s_combo[[cc]]<-s[[i]]
      #temporary holder for spikes
      comboSpikes=s[[i]]$spikes
      #list of channels on initial file (.001)
      initialChannels<-s[[i]]$channels
    } else if (substr(basename(s[[i]]$file),1,
                      nchar(basename(s[[i]]$file))-5)==
                 substr(basename(s_combo[[cc]]$file),
                        1,
                        nchar(basename(s_combo[[cc]]$file))-5) ){
      
      #combine lists together
      comboSpikes<-mapply(c,comboSpikes,
                          s[[i]]$spikes[initialChannels])
      
      #update s_combo
      s_combo[[cc]]$spikes<-comboSpikes
      s_combo[[cc]]$nspikes<-sapply(comboSpikes, length)
      s_combo[[cc]]$rec.time<-c(s_combo[[cc]]$rec.time[1], s[[i]]$rec.time[2])
      s_combo[[cc]]$meanfiringrate <- s_combo[[cc]]$nspikes/
        (s_combo[[cc]]$rec.time[2]-s_combo[[cc]]$rec.time[1])    
      
      
    } else {
      print="issue with coding"
    }
    
  }#end of loop through files
  
  ###make an .h5 file that combines 001, 002, 003 etc appropriately
  #append with _000
  for (i in 1:cc){
    #create file
    h5File_combo<-paste(substr(s_combo[[i]]$file,1,
                               nchar(s[[i]]$file)-7),
                        "_000.h5",sep='')
    if (file.exists(h5File_combo)) 
      unlink(h5File_combo)
    h5createFile(h5File_combo)
    
    #organize data for wells
    wells <- axion.guess.well.number(s_combo[[i]]$channels)
    array <- sprintf("Axion %d well", wells)
    plate.info <- plateinfo(array)
    epos <- axion.elec.name.to.xy(s_combo[[i]]$channels, plate.info)
    
    
    #write data to .h5 file
    h5write(unlist(s_combo[[i]]$spikes), h5File_combo, "/spikes")
    h5write(s_combo[[i]]$nspikes, h5File_combo, "/sCount")
    h5write(epos, h5File_combo, "/epos")
    h5write(s_combo[[i]]$channels, h5File_combo, "/names")
    h5write(array, h5File_combo, "/array")
    h5write(s_combo[[i]]$treatment, h5File_combo, "/treatment")
    h5write(s_combo[[i]]$dose, h5File_combo, "/dose")
    h5write(s_combo[[i]]$genotype, h5File_combo, "/genotype")
    h5write(s_combo[[i]]$pup, h5File_combo, "/pup")
    h5write(s_combo[[i]]$trt.div, h5File_combo, "/trt.div")
    h5write(s_combo[[i]]$well, h5File_combo, "/well")
    h5write(s_combo[[i]]$units, h5File_combo, "/units")
    print(h5ls(h5File_combo))
    
    
  }#end of for loop through s_combo
  h5File_combo
}# end of combine.h5.files.dh
