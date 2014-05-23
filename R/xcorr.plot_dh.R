xcorr.plot_dh <-
function (spikes.a, spikes.b, plot.label = "", xcorr.maxt = 4, 
                         bi = TRUE, nbins = 100, show.poisson = TRUE, autocorr = FALSE, 
                         page.label = date(), pause = TRUE, plot = TRUE, 
                         want.axis = TRUE, plot.ind = FALSE, max.val) {
  # xcorr.maxt is the time before and after we look for correlations between spikes
  # the unit is seconds
  
  #bi controls whether you want to see before the spike happened (TRUE)
  #  or simply after (bi=FALSE)
  
  # blue line is poisson rate of spikes.b (in Hz)
  
  # you take the spikes.a, count the number of times a spike (from spike.b) occurs in multiples of 
  # (xcorr.maxt * 2)/nbins seconds, you have nbins = 100, 
  if (plot.ind == TRUE & !exists("max.val")){
    print("Error: max.val missing and plot.ind = TRUE")
    stop
  }
  
  if (bi) {
    x <- histbi.ab(spikes.a, spikes.b, xcorr.maxt, nbins)
  } else {
    x <- hist.ab(spikes.a, spikes.b, xcorr.maxt, nbins)
  }
  if (autocorr) {
    zero.bin <- floor((nbins/2) + 1)
    x[zero.bin] <- x[zero.bin] - length(spikes.a)
    if (x[zero.bin] < 0) 
      stop(paste("zero.bin cannot be reduced below zero", 
                 x[zero.bin], length(spikes.a)))
  }
  dt.wid <- (2 * xcorr.maxt)/nbins # each bin is this many seconds
  x <- x/(length(spikes.a) * dt.wid) 
  # signif, just gives the number to 2 significant digits
  # e.g. signif(12345, 3) = 12300
  
  # if you're not plotting x-corr of indep. electrodes
  if ( !plot.ind){
    max.val <- signif(max(x), 2) 
    plot.col = "black"
  } else {
    plot.col = "red"
  }
  
  nspikes.b <- length(spikes.b)
  #number of spikes/number of seconds
  poisson.rate <- nspikes.b/(spikes.b[nspikes.b] - spikes.b[1])
  if (plot) {
    plot(x, ylim = c(0, max.val), type = "l", bty = "n", 
         xlab = "second", ylab = "spikes/sec", xaxt = "n", yaxt = "n",
         col = plot.col)
    # plot x = 0
    lines(c(nbins/2, nbins/2), c(0, max.val) )
    
    want.yaxis <- TRUE
    if (want.yaxis) 
      axis(2, at = c(0, max.val), las = 1)
    if (show.poisson) {
      lines(c(1, length(x)), c(poisson.rate, poisson.rate), 
            lty = 1, col = "cyan")
    }
    if (xcorr.plot.xaxistimes) {
      axis(1, c(1, nbins/2, nbins), labels = c(-xcorr.maxt, 
                                               0, xcorr.maxt))
    } else {
      axis(1, c(1, nbins/2, nbins), labels = FALSE)
    }
    mtext(plot.label, side = 3, cex = par()$cex)
    screen.layout <- par()$mfg
    if (identical(all.equal.numeric(screen.layout[1:2], c(1, 
                                                          1)), TRUE)) 
      mtext(page.label, side = 1, outer = TRUE)
    if (identical(all.equal.numeric(screen.layout[1:2], screen.layout[3:4]), 
                  TRUE) && ((names(dev.cur()) == "X11") || (names(dev.cur()) == 
                                                              "windows")) && pause) 
      readline("Press return to see next page of plots.")
  } else {
    x
  }
}
