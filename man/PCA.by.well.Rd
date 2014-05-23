\name{PCA.by.well}
\alias{PCA.by.well}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCA.by.well

}
\description{
Performs PCA on a group of treated and control wells using input summar.  Inputs of csv files with each row a well and each column information about the well.  The csv file must contain columns for "DIV", "date", "trt" and "dose".  

}

\usage{
PCA.by.well(filename.data, trt.params.wanted=NULL, 
                      output.folder, ctr.params.wanted, vars.wanted=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename.data}{
 full file path and name to the .csv file with each well being a row.  If nothing is input, then the user is prompted for input at run time.
}
  \item{trt.params.wanted}{
  a list that must contain "trt.wanted", "DIV.wanted", "dose.wanted"
  where trt.wanted is a vector of character strings of well treatments,
  DIV.wanted is a vector of character strings of DIV wanted,
  dose.wanted is a character string of dose wanted.
  e.g.
  list( trt.wanted, DIV.wanted, dose.wanted )
}
  \item{output.folder}{
 full path to the output folder of where the plots will appear
}
  \item{ctr.params.wanted}{
  a list that must contain "trt.wanted", "DIV.wanted", "dose.wanted" where:
  1. trt.wanted is a vector of character strings the describe which
  elements of "trt" column in filepath.data are control,
  2. DIV.wanted is a vector of character strings of which elements of "DIV"
  column in filename.data wanted,
  3. dose.wanted is a character string of which elements of "dose" column in 
  filepath.data will be used for control
  e.g.
  list( trt.wanted=c("control"), DIV.wanted=c("7","9"), dose.wanted=0 )
}
  \item{vars.wanted}{
 character vector of which numeric columns are desired to be included in PCA analysis.
 Do not include "DIV", "Plate.SN" or "date".  If vars.wanted not specified then all the 
 numeric variables (except "DIV", "Plate.SN" or "date" ) are used in PCA.
}

}

\references{
 
}
\author{
Diana Hall
}

\examples{
\dontrun{ filename.data=system.file("data/AllAEOntogeny_files.csv",  package="meadq", mustWork=T) }
\dontrun{ trt.params.wantedlist = list(DIV.wanted=c("7","9"), 
              trt.wanted=c('Acetaminophen'), dose.wanted=c(1,3,10) ) }
\dontrun{ output.folder = dirname(system.file("data/AllAEOntogeny_files.csv",  
                package="meadq", mustWork=T) ) }
\dontrun{ vars.wanted = c( "dose", "meanfiringrate","burst.per.min",
                  "mean.isis", "per.spikes.in.burst","mean.dur", "mean.IBIs",
                  "nAE", "nABE", "ns.n", "ns.peak.m", "ns.durn.m" )  }
\dontrun{ PCA.by.well(filename.data = filename.data, trt.params.wanted=trt.params.wanted, 
              output.folder = output.folder, ctr.params.wanted = ctr.params.wanted, 
              vars.wanted=vars.wanted ) }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
