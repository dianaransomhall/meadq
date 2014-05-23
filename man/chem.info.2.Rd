\name{chem.info.2}
\alias{chem.info.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
chem.info.2 
}
\description{
Reads in .csv file with meta data for each well. 
Columns of meta data file must include the following names:
\begin{enumerate}
\item Project, a string coding for a particular project or set of recordings 
\item Experiment Date, a date of any format e.g. 20140204 when experiment took place 
\item Plate SN, the plate serial number 
\item DIV, days in vitro the number of days old from the plating of the culture when recording was taken, numeric field
\item well, e.g. "A1", or "F7"
\item treatment, a string describing well treatment
\item size, information about size of treatment
\item dose, dose of treatment
\item units, units of dose 
\end{enumerate}
The .h5 file name should contain the Project, experiment date, plate SN and DIV in that order seperated by a "_" underscore.  e.g. ON_20140205_MW1007-26_DIV02_001.h5 contains necessary information.  Note that any extra information may also be contained in file name, but the suffix of the file must be as described above.
If there is no information accompanying a .csv meta data file, then "NA" should be entered to specify that the omission is purposeful.

}
\usage{
chem.info.2(file, masterChemFile = masterChemFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
 a .mapTimestamp file that one intends to add data to
}
  \item{masterChemFile}{
  a path to the .csv meta data file as described 'descriptions' section

}
}

\references{
see 'examples' in package library for file entitled "logex.csv" for an example of log file
}
\author{
Diana Hall
}

\examples{
\dontrun{ mapTimestampFile<-"ON_20140205_MW1007-26_DIV02_001.mapTimestamps" }
\dontrun{ masterChemFile<-"Experiment_LogFileExample.csv" }
\dontrun{ chem.info.2(mapTimestampFiles, masterChemFile) }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
