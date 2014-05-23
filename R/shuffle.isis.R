shuffle.isis <-
function(spikes){
  
  isi.b = isi(spikes) # get the isi of a spike train
  # take a random sample, of nspikes size, without replacement
  isi.b.rs = sample(isi.b, size = length(isi.b), replace = FALSE, prob = NULL)
  
  new.spikes = c(); cur.spike = spikes[1] #initialize current spike
  for (spike in c(1:length(spikes) ) ){
    #fill in spike time 
    new.spikes = c(new.spikes, cur.spike )
    # update to new spike time
    cur.spike = cur.spike + isi.b.rs[spike] 
  }
  #return new spikes
  new.spikes
}
