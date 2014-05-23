construct.s <-
function(spikes, ids, time.interval, beg, end, corr.breaks, 
    layout, filename) 
{
    spikes.range <- range(unlist(spikes))
    if (!is.null(end)) {
        spikes <- lapply(spikes, jay.filter.for.max, max = end)
    } else {
        end <- spikes.range[2]
    }
    if (!is.null(beg)) {
        spikes <- lapply(spikes, jay.filter.for.min, min = beg)
    } else {
        beg <- spikes.range[1]
    }
    spikes <- remove.empty.channels(spikes)
    if (!is.null(ids)) {
        spikes <- filter.channel.names(spikes, ids)
    }
    channels <- names(spikes)
    keep <- match(channels, rownames(layout$pos))
    layout$pos <- layout$pos[keep, ]
    rec.time <- c(beg, end)
    nspikes <- sapply(spikes, length)
    ###################### patch
    if (length(nspikes) ==0) {
    	   meanfiringrate <- nspikes #empty list
    } else {
	   meanfiringrate <- nspikes/(end - beg)
    }
    ######################
    
    rates <- make.spikes.to.frate(spikes, time.interval = time.interval, 
        beg = beg, end = end)
    unit.offsets <- NULL
    check.spikes.monotonic(spikes)
    res <- list(channels = names(spikes), spikes = spikes, nspikes = nspikes, 
        NCells = length(spikes), meanfiringrate = meanfiringrate, 
        file = filename, layout = layout, rates = rates, unit.offsets = unit.offsets, 
        rec.time = rec.time)
    class(res) <- "mm.s"
    if (length(corr.breaks) == 1) {
        res$corr = NULL
    }    else {
        res$corr = corr.index(res, corr.breaks)
    }
    res
}
