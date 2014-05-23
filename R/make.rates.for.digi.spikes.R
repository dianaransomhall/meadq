make.rates.for.digi.spikes <-
function(dig.s ){
  
  rate = c() ;
  for (cur.ch in 1:length(dig.s) ){
    
    rate = c(rate, sum(dig.s[[cur.ch]])/length(dig.s[[cur.ch]]) )
    
  }
  
  names(rate) = names(dig.s)  
  rate
  
}
