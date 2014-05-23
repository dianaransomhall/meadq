make.joint.digi.s <-
function(dig.s1, dig.s2 ){
  
  # under null hypothesis of independent of channels you'd expect to see a certain group spike rate
  y_temp = dig.s1 + dig.s2 # element wise sum
  
  y_dig = floor( y_temp/2 ) #1 given time.bin if spike occured in both channels, 0 else
  
  
  y_dig
  
}
