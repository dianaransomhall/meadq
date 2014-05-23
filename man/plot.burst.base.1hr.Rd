\name{ plot.burst.base.1hr }
\alias{ plot.burst.base.1hr }
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.burst.base.1hr
}
\description{
take in a spikes object, s, and a attached response of endpoints, e.g. 
s[[ctr.file.ind]]$meanfiringrate and then plots the control or baseline file on top of the 
treatment file (ie the file index in spike object s by trt.file.ind)

}
\usage{
plot.burst.base.1hr( s , resp , resp.label, ctr.file.ind=1 , trt.file.ind = 2 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{  s }{
 an .R file that contains parameter data such as max interval burst detection algorithm parameters. Default is "NULL" trigers prompt for user to enter parameter file location
}
  \item{resp}{
  any set of endpoints attached to s[[ctr.file.ind]] & s[[trt.file.ind]].  e.g. "meanfiringrate"
  or "cv.time"
}
  \item{ resp.label }{
  a string that gives the label for the response chosen, e.g. "MFR" for meanfiringrate
}

  \item{ ctr.file.ind }{
  an index of which file within the spike object (s) e.g. ctr.file.ind = 2, which means
  s[[ctr.file.ind]] must exist. Default: ctr.file.ind=1
}

  \item{ trr.file.ind }{
  an index of which file within the spike object (s) e.g. ctr.file.ind = 3, which means
  s[[trt.file.ind]] must exist. Default: trt.file.ind=2
}



}

\references{

}
\author{
Diana Hall
}

\examples{

\dontrun{ plot.burst.base.1hr( s , resp="meanfiringrate" ,  resp.label="MFR", ctr.file.ind=1 , trt.file.ind = 2 ) }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
