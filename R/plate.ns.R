plate.ns <-
function(s, t.p){
  

    
    ns=list()
    ns[[ length( s[[t.p]]$goodwells ) ]] = 0
    for (well in c(1:length( s[[t.p]]$goodwells ) ) ){
      
      ns[[well]] <- compute.ns(s[[t.p]], ns.T=0.003, ns.N=6, 
                               sur=100, whichcells=s[[t.p]]$goodwells[well]  )
      ns[[well]]$well = paste( s[[t.p]]$goodwells[well] )
      print (paste("well ", paste(s[[t.p]]$goodwells[well]), " finished" )  )
      
    } #end of for loop through wells in a plate
    ns
  
}
