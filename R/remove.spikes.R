remove.spikes <-
function(s, ids) {
  ## ids=vector of indicies eg c(1,2,4,5)
  ## Remove spikes listed in IDS from S data structure, and return
  ## new structure.
  
  beg <- s$rec.time[1]
  end <- s$rec.time[2]
  corr.breaks <- 0                      #TODO: hardcoded for axion!
  layout <- s$layout
  filename <- paste0(s$file, ".edited")
  s2 <- construct.s(s$spikes, ids, s$rates$time.interval, beg, end,
                    corr.breaks, layout, filename)
  s2
}
