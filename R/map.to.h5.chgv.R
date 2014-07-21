map.to.h5.chgv<-function (spikes, chem.info, h5file) {
  h5file <- path.expand(h5file)
  if (file.exists(h5file)) unlink(h5file)
  nspikes <- sapply(spikes, length)
  channels <- names(spikes)
  well<-chem.info$well
  treatment<-chem.info$treatment
  dose<-chem.info$dose
  units<-chem.info$units
  genotype<-chem.info$genotype
  pup<-chem.info$pup
  trt.div<-chem.info$trt.div
  wells <- axion.guess.well.number(channels)
  
  
  array <- sprintf("Axion %d well", wells)
  plateinfo <- plateinfo(array)
  epos <- axion.elec.name.to.xy(channels, plateinfo)
  
  print("epos <- axion.elec.name.to.xy(channels, plateinfo)")
  
  h5createFile(h5file)
  sum.spikes <- sum(nspikes)
  h5write(unlist(spikes), h5file, "/spikes")
  h5write(nspikes, h5file, "/sCount")
  h5write(epos, h5file, "/epos")
  h5write(channels, h5file, "/names")
  h5write(array, h5file, "/array")
  h5write(treatment, h5file, "/treatment")
  h5write(dose, h5file, "/dose")
  h5write(units, h5file, "/units")
  h5write(genotype, h5file, "/genotype")
  h5write(pup, h5file, "/pup")
  h5write(trt.div, h5file, "/trt.div")
  h5write(well, h5file, "/well")
  
  print(h5ls(h5file))
}#end of map.to.h5.chgv



