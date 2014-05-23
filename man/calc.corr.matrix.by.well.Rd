\name{calc.corr.matrix.by.well}
\alias{calc.corr.matrix.by.well}

\title{
calc.corr.matrix.by.well
}
\description{
creates a matrix for a give spike object, recording time, well and time bin
}

\usage{
calc.corr.matrix.by.well(s, t.p, well, time.bin)
}

\arguments{
  \item{s}{
a spike object that's a list with one entry for each recording.
}
  \item{t.p}{
a numeric value indicating which recording is desired in list of 
recordings
}
  \item{well}{
a string indicating which well is desired. e.g. "A3"
}
  \item{time.bin}{
a number of ms desired to compute bin length
}

}

\details{ 
Must have spikes member, s[[t.p]]$spikes; 
recording time, s[[t.p]]$rec.time
channels, s[[t.p]]$channels
well for each channel, s[[t.p]]$cw
}


\note{
The matrix has row names and column names inicating channels 
on which correlation was computed

}


\examples{
\dontrun{load(system.file("data/example_ont_data.Rdata",  package="meadq", mustWork=T)) }
\dontrun{ f=calc.corr.matrix.by.well(s, t.p=3, well="A3", time.bin=5) }


}
 
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
