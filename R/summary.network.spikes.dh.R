summary.network.spikes.dh <-
function(e,nspikes,ns.E,sur){
  wells<-nspikes$wells
  
  names(wells) <- wells #keep the names valid.
  for (i in 1:length(wells)) {
    well <- wells[[i]]
    data <- nspikes$ns.all[[i]]$measures
    # NULL has length 0, while NA has length 1
    if (length(data)==0){
      data.present=FALSE
    } else if ( is.na(data)[1]  ){
      data.present=FALSE
    } else {
      data.present=TRUE
    }
    
    if (data.present ) {
      indexes <- names.to.indexes(names(e$spikes), well, allow.na = TRUE)
      electrodes <- names(e$spikes)[indexes]
      en.map <- matrix(0,length(electrodes),dim(data)[1])
      rownames(en.map) <- electrodes
      colnames(en.map) <-  as.character(data[,1])
      #beg <- floor(min(unlist(e$spikes[indexes])))
      for (ns in 1:dim(data)[1]) {
        current.ns <- data[ns,1]
        en.map[,ns] <- unlist(lapply(e$spikes[indexes],
                                     function(x) {length(x[x>current.ns & x<= current.ns + (e$ns.all)[[i]]$ns.T])}))
      }
      
      en.map[en.map < ns.E] <- 0
      filtered.indexes <- which(colSums(en.map>=ns.E) >= (e$ns.all)[[i]]$ns.N)
      
      en.map <- en.map[,filtered.indexes]
      if (length(filtered.indexes) == 1) {  #deal with R vector matrix problem
        dim(en.map) <- c(length(electrodes),length(filtered.indexes))
        colnames(en.map) <-  names(filtered.indexes)
        rownames(en.map) <- electrodes
      }       
      
      if (dim(en.map)[2] > 0) {
        p <- data[filtered.indexes,1:2]
        dim(p) <- c(length(filtered.indexes),2)
        colnames(p)<-c("time","index")
      } else {
        p <- NULL
      }
    } else {
      p <- NULL
    }
    
    ns <- list(counts = nspikes$ns.all[[i]]$counts, 
               ns.N = (nspikes$ns.all)[[i]]$ns.N, 
               ns.T = (nspikes$ns.all)[[i]]$ns.T)
    class(ns) <- "ns"
    m <- mean.ns(ns, p, plot = FALSE, nrow = 4, ncol = 4, ask = FALSE, sur = sur)
    if (is.null(m)) {
      ns$brief <- c(n = 0, peak.m = NA, peak.sd = NA, durn.m = NA,durn.sd = NA,
                    percent.of.spikes.in.ns = NA,
                    mean.spikes.in.ns = NA,
                    mean.insi = NA)
      ns$en.map <- NULL
      ns$brief.electrode <- NULL
    } else {
      ns$mean <- m$ns.mean
      ns$measures <- m$measures
      peak.val <- ns$measures[, "peak.val"]
      durn <- ns$measures[, "durn"]
      insis <- diff(ns$measures[, "time"])
      if (length(insis) > 0) {
        mean.insis <- mean(insis)
      } else {
        mean.insis <- NA
      }
      ns$brief <- c(n = nrow(ns$measures), peak.m = mean(peak.val), 
                    peak.sd = sd(peak.val), durn.m = mean(durn, na.rm = TRUE), 
                    durn.sd = sd(durn, na.rm = TRUE),
                    percent.of.spikes.in.ns = 100*sum(en.map)/sum(e$nspikes[indexes]),
                    mean.spikes.in.ns = sum(en.map) / nrow(ns$measures),
                    mean.insis = mean.insis
      )
      if ( dim(en.map)[2] != dim(ns$measures)[1]) {
        en.map <- en.map[,as.character(ns$measures[,1])]
      }
      ns$en.map <- en.map
      
      #now briefs at electrode level
      features <- c("spikes","ns","spikes.in.ns","percent.of.spikes.in.ns",
                    "mean.spikes.per.ns","sd.spikes.per.ns","mean.insis")
      en.brief <- matrix(0,dim(en.map)[1],length(features))
      rownames(en.brief) <- rownames(en.map)
      colnames(en.brief) <- features
      en.brief[,"spikes"] <- e$nspikes[indexes]
      en.brief[,"ns"] <-  rowSums(en.map>0)
      en.brief[,"spikes.in.ns"] <-  rowSums(en.map)
      en.brief[,"percent.of.spikes.in.ns"] <-  100 * en.brief[,"spikes.in.ns"] / en.brief[,"spikes"]
      
      en.brief[,"mean.spikes.per.ns"] <-  en.brief[,"spikes.in.ns"] / en.brief[,"ns"]
      en.brief[,"sd.spikes.per.ns"] <- unlist(lapply(rownames(en.brief), function(e) {
        temp <- sd(en.map[e,which(en.map[e,] >0)])
        temp[is.na(temp)] <- NaN
        temp
      }))
      
      
      en.brief[,"mean.insis"] <- unlist(lapply(rownames(en.brief), function(e) {
        insis <- diff(as.numeric(names(which(en.map[e,] >0))))
        m <- mean(insis)
        m
      }))
      
      en.brief[is.nan(en.brief)] <- NA
      
      ns$en.brief <- en.brief
    }
    
    nspikes$ns.all[[i]] <- ns
  }
  nspikes
}
