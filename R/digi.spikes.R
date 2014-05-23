digi.spikes <-
function( s, time.bin, t.p, w ){

    #get indices of current well
    if (is.character(w)){
      index.w=which(s[[t.p]]$cw==w )  
      
    } else {
      index.w=which(s[[t.p]]$cw==unique(s[[t.p]]$cw)[w] )
      
    }
    # get which channels are wanted
    channels.wanted = s[[t.p]]$channels[index.w]  
  
    #convert seconds to ms
    start.ms=round(s[[t.p]]$rec.time[1]*10^3,digits=0)
    stop.ms=round(s[[t.p]]$rec.time[2]*10^3,digits=0)
    total.ms=stop.ms-start.ms
    
    # get total number of bins
    total.bins = ceiling( total.ms/time.bin )   
    
    #digitize spikes
    dig.s=list()
    dig.s[[length(index.w)]]=rep(0, total.bins )
    names(dig.s)=channels.wanted
       
    #digitize spikes: loop through channels
    for (cur.ch in channels.wanted){
      
      dig.s[[cur.ch]]=rep(0, total.bins) # create a digital spike train
      
      # get current spike train in ms
      cur.nspikes = s[[t.p]]$nspikes[[cur.ch ]]
      
      # get spikes with initial time removed in ms
      cur.spike.train = 10^3 *( s[[t.p]]$spikes[[ cur.ch ]] - s[[t.p]]$rec.time[1] )
      
      for (cur.spike.ind in 1:cur.nspikes ){
        # get bin for spike
        cur.bin = ceiling(cur.spike.train[cur.spike.ind]/time.bin )
        #get millisecond
        dig.s[[cur.ch]][ cur.bin ] = 1
      }
    }
    
  dig.s
    
}
