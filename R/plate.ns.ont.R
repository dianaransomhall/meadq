plate.ns.ont <-
function(s, t.p, ns.T, ns.N, sur ){
  
  ns=list()
  ns[[ length(unique( s[[t.p]]$cw ) ) ]] = c()
  for ( cur.well in unique( s[[t.p]]$cw ) ){
    
    ns[[cur.well]] <- compute.ns(s[[t.p]], ns.T=ns.T, ns.N=ns.N, 
                                 sur=sur, whichcells=cur.well  )
    ns[[cur.well]]$well = cur.well
    print (paste("well ", cur.well, " finished" )  )
    
  } #end of for loop through wells in a plate
  ns
  
}
