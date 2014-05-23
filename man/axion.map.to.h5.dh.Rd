\name{axion.map.to.h5.dh}
\alias{axion.map.to.h5.dh}

\title{
axion.map.to.h5.dh
}
\description{
Take .mapTimestamp files (.map files from axion biosystems MEA, then exported from neuroExplorer into a .mapTimestamp files )
}
\usage{
axion.map.to.h5.dh(key,  chem.info)
}
\arguments{
  \item{key}{
 key<-strsplit(basename(mapTimestampFile), ".", fixed = TRUE)[[1]][1]
}
  \item{chem.info}{
chem.info<-chem.info.2(mapTimestampFile,masterChemFile)
}

}


\author{
Diana Hall
}
\note{
h5.dir, the directory where .h5 files will appear, must be assigned before the functions are run.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# load necessary files 

\dontrun{ h5.dir<-"C:/Users/h5Files" } # example directory

# load mapTimestampFile
\dontrun{ mapTimestampFile<-"C:/Users/example/file1.mapTimestamps" }
# experimental log file
\dontrun{ masterChemFile<-"Experiment_LogFileExample.csv" }

\dontrun{ title <-strsplit(basename(mapTimestampFile), ".", fixed = TRUE }
\dontrun{ chem.info<-chem.info.2(mapTimestampFile,masterChemFile) }

\dontrun{ axion.map.to.h5.dh(key = title, chem.info = chem.info) } 

}
% Add one or more standard  keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
