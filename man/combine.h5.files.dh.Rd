\name{combine.h5.files.dh}
\alias{combine.h5.files.dh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combines .h5 spike objects by categories within spike object
}
\description{
Combines .h5 spike objects by categories within spike object
}
\usage{
combine.h5.files.dh(h5Files)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5Files}{
%%     ~~Describe \code{h5Files} here~~
A vector of .h5 file paths to be stacked
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~


}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}


\examples{

\dontrun{h5Files=c(system.file("/example/ON_20140205_MW1007-26_DIV05_001.h5", package="meadq"), 
  system.file("/example/ON_20140205_MW1007-26_DIV07_001.h5", package="meadq"))}

\dontrun{ cs<-combine.h5.files.dh(h5Files) }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
