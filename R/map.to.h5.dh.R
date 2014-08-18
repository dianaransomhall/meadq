map.to.h5.dh <-
function (spikes, chem.info, h5file) 
{
  sjemea::map.to.h5(spikes, h5file)
  h5file <- path.expand(h5file)
  
  #if (file.exists(h5file)) unlink(h5file)
  channels<- names(spikes)
  h5write(channels, h5file, "/channels")
  
  well<-chem.info$well
  h5write(well, h5file, "/well")
  
  wells <- axion.guess.well.number(channels)
  array <- sprintf("Axion %d well", wells)
  
  if( length(chem.info$treatment)>1 ){
    treatment<-chem.info$treatment
    h5write(treatment, h5file, "/treatment")
  }
  
  if( length(chem.info$dose)>1 ){
    dose<-chem.info$dose
    h5write(dose, h5file, "/dose")
  }
  
  if( length(chem.info$units)>1 ){
    units<-chem.info$units
    h5write(units, h5file, "/units")
  }
  
  if( length(chem.info$size)>1 ){
    size<-chem.info$size
    h5write(size, h5file, "/size")
  }

  h5write(array, h5file, "/array")
  
  print(h5ls(h5file))
}
