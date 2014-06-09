\name{ get.burst.info.averaged.over.well }
\alias{ get.burst.info.averaged.over.well }
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get.burst.info.averaged.over.well
}
\description{
 takes a spike object with or w/o associated burst data and summarized all variables across 
 channels within well.  data can be summaries only from 'good' wells or summary of each well in 
 recording may be obtains.

}
\usage{
get.burst.info.averaged.over.well(s, goodwells.ind=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ s }{
 s is a spike object: a list of spikes that have associated meta data and may have other fields 
 populated such as burst info.
}
  \item{ goodwells.ind }{
 boolean value. TRUE= if you have filtered the wells and an associated vector of well names exist called
 'goodwells'. e.g. s[[2]]$goodwells = c("A1", "A3", "F8"). Default is true
}
}

\author{
Diana Hall
}

\examples{

\dontrun{ get.burst.info.averaged.over.well( s, goodwells.ind=F ) }

}
 
