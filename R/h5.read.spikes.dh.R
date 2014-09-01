h5.read.spikes.dh <-
function (h5file, ids = NULL, time.interval = 1, beg = NULL, 
                             end = NULL, corr.breaks) 
{
  chop <- function(v, counts) {
    stopifnot(sum(counts) == length(v))
    end <- cumsum(counts)
    beg <- c(1, 1 + end[-length(end)])
    begend <- cbind(beg, end)
    apply(begend, 1, function(x) v[x[1]:x[2]])
  }
  #reads in data: 
  data <- h5read(path.expand(h5file), name = "/")
  #chop breaks up the spikes into their respective channels
  spikes <- chop(as.vector(data$spikes), data$sCount)
  names(spikes) <- data$names
  #get.array.info() expects the names to just have each their channel title
  arrayinfo <- get.array.info(data)
  layout <- arrayinfo$layout
  if (missing(corr.breaks)) {
    corr.breaks <- arrayinfo$corr.breaks
  }
  s <- construct.s(spikes, ids, time.interval, beg, end, corr.breaks, 
                   layout, filename = h5file)
  
  names.data<-names(data)
  if (is.element("dose", names.data) ){
    s$dose<-data$dose
  }
  if (is.element("treatment", names.data) ){
    s$treatment<-data$treatment
  }
  if (is.element("units", names.data) ){
    s$units<-data$units
  }
  if (is.element("well", names.data) ){
    s$well<-data$well
  }
  if (is.element("genotype", names.data) ){
    s$genotype<-data$genotype
  }
  if (is.element("pup", names.data) ){
    s$pup<-data$pup
  }
  if (is.element("trt.div", names.data) ){
    s$trt.div<-data$trt.div
  }

  
  s<-get.num.AE(s)
  
  s
}
