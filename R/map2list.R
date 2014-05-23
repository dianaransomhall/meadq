map2list <-
function (file) 
{
  data <- scan(file, what = character(), sep = "\t")
  max.channels <- 1997
  channels <- grep("[ABCDEF]", data[1:max.channels])
  nchannels <- length(channels)
  stopifnot(nchannels != max.channels)
  names <- substring(data[channels], 1,5)
  well <- as.factor(substring(names, 1, 2))
  data3 <- data[-c(1:nchannels, length(data))]
  longest.spike <- length(data3)/nchannels
  spikes.matrix <- matrix(data3, nrow = longest.spike, byrow = T)
  spikes <- apply(spikes.matrix, 2, function(x) sjemea::jay.filter.for.na(as.numeric(x)))
  names(spikes) <- names
  spikes
}
