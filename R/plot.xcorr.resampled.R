plot.xcorr.resampled <-
function(s, t.a, t.b, t.p, xcorr.maxt=4 ){
  
  
  
  # two trains from different wells have no 
  
  
  spikes.a = s[[t.p]]$spikes[[t.a]];   spikes.b = s[[t.p]]$spikes[[t.b]] ; 
  name.a =  s[[t.p]]$channel[[t.a]] ;  name.b =  s[[t.p]]$channel[[t.b]] ;  
  
  
  
  
  plot.label= paste(
    paste("X-corr of ",name.a, " to ", name.b, "  ymax= lag (s) of best signal alignment ", sep=""), 
    "b-line: MFR of 2nd spike train   r-line: X-corr of 2nd train shuffled " , sep="\n") 
  
  
  
  # first get max(y) to be able to overlay independent electrode x-corr
  temp.x = xcorr.plot_dh(spikes.a, spikes.b, plot.label, xcorr.maxt = xcorr.maxt, show.poisson = F, 
                         want.axis=F, plot.ind = FALSE, plot=FALSE)
  max.val = signif(max(temp.x), 2) # get the max from the histogram, that will be input for plot later
  
  par(mar=c(4,3,3,1))
  
  # now plot the x-corr of interest
  xcorr.plot_dh(spikes.a, spikes.b, plot.label, xcorr.maxt = xcorr.maxt, show.poisson = T, 
                want.axis=T, plot.ind = FALSE, plot=TRUE)
  
  par(new=TRUE) # equivalent to "hold on" in matlab
  
  # get spike trains from different wells (i.e. independent)
  spikes.b.shuffled = shuffle.isis(spikes.b)
  
  # replot with different 
  xcorr.plot_dh(spikes.a, spikes.b.shuffled, plot.label, xcorr.maxt = xcorr.maxt, show.poisson = F, 
                want.axis=F, plot.ind = TRUE, plot=TRUE, max.val = max.val )
  
  
}
