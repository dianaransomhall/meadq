map.to.h5.dh <-
function (spikes, chem.info, h5file) 
{
  sjemea::map.to.h5(spikes, h5file)
  h5file <- path.expand(h5file)
  
  #if (file.exists(h5file)) unlink(h5file)
  channels<- names(spikes)
  well<-chem.info$well
  treatment<-chem.info$treatment
  size<-chem.info$size
  dose<-chem.info$dose
  units<-chem.info$units
  wells <- axion.guess.well.number(channels)
  array <- sprintf("Axion %d well", wells)
  
  h5write(array, h5file, "/array")
  h5write(channels, h5file, "/channels")
  h5write(treatment, h5file, "/treatment")
  h5write(dose, h5file, "/dose")
  h5write(size, h5file, "/size")
  h5write(well, h5file, "/well")
  h5write(units, h5file, "/units")
  print(h5ls(h5file))
}
