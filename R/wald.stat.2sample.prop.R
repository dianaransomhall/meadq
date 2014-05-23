wald.stat.2sample.prop <-
function(nsucess1, nsucess2, n1, n2 ){
  
  p1 = nsucess1/n1 ;  p2 = nsucess2/n2; p.all = (nsucess1+nsucess2)/(n1+n2)
  
  denom = sqrt( (p.all* (1 - p.all) )*( (1/n1)+(1/n2) ) )
  temp = ( p1 - p2 )/ denom
  
  temp2 = dnorm(temp, 0,1) # get p-value but comparing to N(0,1)
  
  z.w = c(temp, temp2)
  names(z.w) = c("Wald 2-sample test value", "p-value")
  z.w
}
