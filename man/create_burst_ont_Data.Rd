\name{ create_burst_ont_Data }
\alias{ create_burst_ont_Data }
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
create_burst_ont_Data
}

\description{
Function creates a .csv file where each column is a different variable (e.g. burst per minute) and each row is one well from one of the .h5 files provided.
}

\usage{
create_burst_ont_Data(h5Files, save.rdata = F)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{h5Files}{

A list of paths to .h5 files.  Must be specified.
}
  \item{save.rdata}{
setting to true TRUE creates a .rdata file in the same directory where the first .h5 file came from. FALSE makes no .rdata file.
}
}

\details{

.csv contains a variety of variables and may be used to easily read in and graph. Two .csv files are created. 
1. AE file (AE:= active electrodes ) is created using averages across all channels that are active, when applicable.  For example, nAE (# active electrodes) is a count, but bursts per minute is averaged across only active electrodes within the well.
2. ABE file is created by averaging variables across all channels that are actively bursting (defined as at least 0.5 bursts/minute), when applicable.

}


\examples{

\dontrun{ h5Files=system.file( "inst/ON_20140205_MW1007-26_DIV09_001.h5",package="meadq", mustWork=T ) }
\dontrun{ create_burst_ont_Data(h5Files = h5Files, save.rdata=T )  }


}


