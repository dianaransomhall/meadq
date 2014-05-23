\name{ burst_table_Plots_comp }
\alias{burst_table_Plots_comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
burst_table_Plots_comp
}
\description{
Reads in .h5 files or .rdata spike files. prompts user for parameter file that contains parameters for max interval burst detection algorithm.  .pdf file containing layout info of each file passed, as well as quantitative graphs of firing rates etc.
The .h5 file name should contain the Project, experiment date, plate SN and DIV in that order seperated by a "_" underscore.  e.g. ON_20140205_MW1007-26_DIV02_001.h5 contains necessary information.  Note that any extra information may also be contained in file name, but the suffix of the file must be as described above.

}
\usage{
burst_table_Plots_comp(param.file=NULL, h5Files = filepath )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param.file}{
 an .R file that contains parameter data such as max interval burst detection algorithm parameters. Default is "NULL" trigers prompt for user to enter parameter file location
}
  \item{h5Files}{
  a list of paths to h5Files.  Default is "NULL" and trigers prompt for user to select .h5Files

}
}

\references{

}
\author{
Diana Hall
}

\examples{

\dontrun{ burst_table_Plots_comp( ctr.file.ind=1, trt.file.ind = 3 ) }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
