\name{xcorr.plot_dh}
\alias{xcorr.plot_dh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
xcorr.plot_dh
}

\description{
Takes two spike trains (or one for autocorrelogram) and forms a cross correlogram
}

\usage{
xcorr.plot_dh(spikes.a, spikes.b, plot.label = "", 
xcorr.maxt = 4, bi = TRUE, nbins = 100, 
show.poisson = TRUE, autocorr = FALSE, page.label = date(), 
pause = TRUE, plot = TRUE, want.axis = TRUE, plot.ind = FALSE, max.val)
}


\arguments{
  \item{spikes.a}{
%%     ~~Describe \code{spikes.a} here~~
spike train
}
  \item{spikes.b}{
%%     ~~Describe \code{spikes.b} here~~
spike train. If spikes.a = spikes.b then a auto-corellogram is output.
}

  \item{plot.label}{
%%     ~~Describe \code{plot.label} here~~
Title for plot
}
  \item{xcorr.maxt}{
%%     ~~Describe \code{xcorr.maxt} here~~
the number of seconds before and after occurence of reference spike displayed in plot.
May be a decimal in the case that ms specification is desired.  Default = 4

}
  \item{bi}{
%%     ~~Describe \code{bi} here~~
TRUE if histogram of values over [-xcorr.maxt, xcorr.maxt] is desired.
FALSE if histogram of [0 xcorr.maxt] is desired.  
Default = TRUE
}
  \item{nbins}{ 
%%     ~~Describe \code{nbins} here~~
the bins in the histogram.  The larger the number the finer the plot will appear.
Default = 100.
}
  \item{show.poisson}{
%%     ~~Describe \code{show.poisson} here~~
TRUE to display line representing firing rate of spikes.b on plot. 
Default = TRUE 
  \item{autocorr}{
%%     ~~Describe \code{autocorr} here~~
TRUE for autocorrelogram.  Note: spikes.a == spikes.b for autocorr.
Default = FALSE
}
  \item{page.label}{
%%     ~~Describe \code{page.label} here~~
a string to be displayed in plot.  Default will display current date.
}
  \item{pause}{
%%     ~~Describe \code{pause} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
TRUE if a plot is desired.
}
  \item{want.axis}{
%%     ~~Describe \code{want.axis} here~~
}
  \item{plot.ind}{
%%     ~~Describe \code{plot.ind} here~~
}
  \item{max.val}{
%%     ~~Describe \code{max.val} here~~
sets the limit of the y-axis shown on plot.
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
\references{
enter ?hist.ab or ?histbi.ab for related documentation
}
\author{
Diana Hall
}


\examples{

\dontrun{load(system.file("data/example_ont_data.Rdata",  package="meadq", mustWork=T)) }
\dontrun{spikes.a = s[[3]]$spikes[[5]]; spikes.b=s[[3]]$spikes[[7]] }
\dontrun{ xcorr.plot_dh( spikes.a, spikes.b, xcorr.maxt = 1, nbins=50) }

}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
