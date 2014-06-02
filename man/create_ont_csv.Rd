\name{create_ont_csv}
\alias{create_ont_csv}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
create_ont_csv
}

\description{
wrapper for create_burst_ont_Data, which prompts for param files, and .h5 files. Function creates a .csv file where each column is a different variable (e.g. burst per minute) and each row is one well from one of the .h5 files provided.
}

\usage{
create_burst_ont_Data(h5Files, save.rdata = F, param.file = NULL)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{h5Files}{

A list of paths to .h5 files.  If not specified or =NULL then the user will be prompted to input the .h5Files; multiple may be entered at once, by holding down CTRL key during selection.  Param file will be prompted for.

}
  \item{save.rdata}{
setting to true TRUE creates a .rdata file in the same directory where the first .h5 file came from. FALSE makes no .rdata file.
}

  \item{param.file }{
The parameters applied to data for max interval burst detection.  If NULL, then default parameters that come with pacakge are used.
}

}

\details{

.csv contains a variety of variables and may be used to easily read in and graph. Two .csv files are created. 
1 AE file (AE:= active electrodes ) is created using averages across all channels that are active, when applicable.  For example, nAE (# active electrodes) is a count, but bursts per minute is averaged across only active electrodes within the well.
2. ABE file is created by averaging variables across all channels that are actively bursting (defined as at least 0.5 bursts/minute), when applicable.

}


\examples{

\dontrun{ h5Files="C:/Users/Example_h5_Files/File1.h5" }
\dontrun{ create_ont_csv(h5Files = NULL, save.rdata=T )  }


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
