axion.map.to.h5.dh.old <-
function (key, chem.info, make.venn = FALSE, debug = TRUE) 
{
  #makes file name for .h5 files from existing .mapTimestamp file name
  h5file <- sprintf("%s/%s.h5", h5.dir, key)
  wildcard <- sprintf("^%s.*mapTimestamps(.xz)?$", key)
  #f is a list of all files
  f <- list.files(path = data.dir, pattern = wildcard, full.names = TRUE)
  if (debug) {
    print(f)
  }
  #get spikes
  spikes.sep <- lapply(f, map2list.old)
  short.filenames <- gsub(".mapTimestamps", "", basenamepy(f)$base)
  summary.table <- t(sapply(spikes.sep, axion.spikesum2))
  rownames(summary.table) <- short.filenames
  ma <- do.call("rbind", lapply(spikes.sep, axion.spikestodf))
  #s2 is a list with all the channels and spikes under each channel
  s2 <- split(ma$time, ma$elec)
  numelec <- length(s2)
  total.spikes <- sum(sapply(s2, length))
  time.ranges <- sapply(s2, range)
  time.min <- min(time.ranges[1, ])
  time.max <- max(time.ranges[2, ])
  #printf formats the text and variables to output ready formats
  #cat contatenates files and then prints them
  cat(printf("Total number of spikes: %d\n", total.spikes))
  cat(printf("Unique number of electrodes: %d\n", numelec))
  cat(printf("Time range [%.3f %.3f] (seconds)\n", time.min, 
             time.max))
  print(summary.table)
  #map.to.h5.dh does the writting to the .h5 file
  #h5file is a filename of output file
  #s2 is object that contains data, chem.info (passed into function) is a info about
  #chemicals in specific wells in files
  map.to.h5.dh(s2, chem.info, h5file)
  if (debug) {
    d <- as.data.frame(summary.table)
    d2 <- data.frame(file = rownames(summary.table), d, 
                     stringsAsFactors = FALSE)
    h5write(d2, path.expand(h5file), "summary.table")
  }
  if (make.venn && is.element(length(f), c(2, 3))) {
    require(VennDiagram)
    require(grid)
    if (length(f) == 2) {
      elec1 <- names(spikes.sep[[1]])
      elec2 <- names(spikes.sep[[2]])
      setdiff(elec1, elec2)
      setdiff(elec2, elec1)
      cat.pos <- c(-10, 10)
    }
    if (length(f) == 3) {
      elec1 <- names(spikes.sep[[1]])
      elec2 <- names(spikes.sep[[2]])
      elec3 <- names(spikes.sep[[3]])
      setdiff(elec1, elec2)
      setdiff(elec1, elec3)
      setdiff(elec2, elec1)
      setdiff(elec2, elec3)
      cat.pos <- c(-10, 10, 180)
    }
    x = lapply(spikes.sep, names)
    sapply(x, length)
    names(x) <- short.filenames
    pdf(file = sprintf("%s_venn.pdf", key))
    p <- venn.diagram(x = x, euler.d = FALSE, scaled = FALSE, 
                      cat.pos = cat.pos, filename = NULL, reverse = FALSE)
    grid.newpage()
    grid.draw(p)
    dev.off()
  }
  h5file
}
