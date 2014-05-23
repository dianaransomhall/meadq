


elec.min.rate <- 0
elec.max.rate <- 1000
well.min.rate <- 0


mi.par <- list(beg.isi =    0.1,
               end.isi =    0.25,
               min.ibi =    0.8,
               min.durn =   0.05,
               min.spikes = 5)


ns.T <- 0.05    	#time in seconds
ns.N <- 4         #how many coincident electrodes?
sur<-100 # num. ms before and after spike to check I think, used in ms




filename.data<-"F:/Kathleen/Rpackage/meadq/data/AllAEOntogeny_files.csv"

output.folder<- "F:/Kathleen/Analysis/PCA/"
trt.params.wanted = list(DIV.wanted=c("7","9"), 
                         trt.wanted=c('Acetaminophen'), 
                         dose.wanted=c(1,3,10) )
ctr.params.wanted = list(DIV.wanted=c("7","9"), 
                         trt.wanted=c('Acetaminophen'), 
                         dose.wanted=c(0) )
vars.wanted = c( "dose", "meanfiringrate","burst.per.min", "mean.isis", "per.spikes.in.burst",
                 "mean.dur", "mean.IBIs", "nAE", "nABE", "ns.n", "ns.peak.m", "ns.durn.m" )  




