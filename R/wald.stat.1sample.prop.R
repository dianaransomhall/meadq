wald.stat.1sample.prop <-
function(p_0, p_sample, n ){
  
  denom = sqrt( (p_sample* (1 - p_sample) )/ n )
  temp = ( p_sample - p_0 )/ denom
  
  temp2 = dnorm(temp, 0,1) # get p-value but comparing to N(0,1)
  
  z.w = c(temp, temp2)
  names(z.w) = c("Wald test value", "p-value")
  z.w
}
