
## ----install-sjemea, eval=FALSE, echo=TRUE-------------------------------
## tarfile.path = "F:/R/Rpackage_meadq/meadq_1.0.1.tar.gz"
## install.packages(pkgs = tarfile.path, type="source", repos = NULL)
## vignette('meadq-intro', package='meadq')


## ----install-sjemea-github,eval=FALSE,echo=TRUE--------------------------
## install.package("devtools") # note: need R v.>=3.1.0
## # to install devtools, use CRAN
## require(devtools)
## install_github("dianaransomhall/meadq")


## ----vignette, eval=FALSE, echo=TRUE-------------------------------------
## vignette('meadq-intro', package='meadq')


## ----setup, message=FALSE------------------------------------------------
require(meadq)
require(knitr)
opts_chunk$set(cache=TRUE)
opts_chunk$set(dev='pdf')


## ----help-pages, eval=FALSE, message=FALSE-------------------------------
## help(package='meadq')


## ----create-h5-files, eval = F-------------------------------------------
## ##
## data.file <- system.file("extdata", "ON_20140205_MW1007-26_DIV07_001.mapTimestamps", package="meadq")
## # user will be promted to navigate to .mapTimestamps files & log file
## make.axion.map.to.h5.dh()


## ----create-well-summary, eval=FALSE-------------------------------------
## data.file1 <- system.file("extdata", "ON_20140205_MW1007-26_DIV05_001.h5", package = "meadq")
## data.file2 <- system.file("extdata", "ON_20140205_MW1007-26_DIV07_001.h5", package = "meadq")
## data.file3 <- system.file("extdata", "ON_20140205_MW1007-26_DIV09_001.h5", package = "meadq")
## h5Files = c(data.file1, data.file2, data.file3)
## param.file <<- system.file( "data","chgv_parameters.rda", package = "meadq")
## 
## create_ont_csv(h5Files = h5Files, save.rdata = TRUE, param.file = param.file )


## ----rdata, eval=F-------------------------------------------------------
## data.file <- system.file("data", "example_ont_data.rda", package = "meadq")
## load(data.file)
## 


## ----bursts-load---------------------------------------------------------
data("example_ont_data")


## ----show-burst-info-----------------------------------------------------
s[[3]]$channels[[2]]
head(s[[3]]$allb[[2]])
nbursts <- sapply(s[[3]]$allb, nrow)
plot(nbursts, xlab='Electrode number', ylab='Number of bursts',
     bty='n', las=1)


## ----burst-raster--------------------------------------------------------
plot(s[[2]], beg=200, end=250, show.bursts=TRUE, whichcells=1:5)


## ----Summary-Plots, eval = FALSE-----------------------------------------
## h5Files = system.file("data", "example_ont_data.rda", package = "meadq")
## param.file = system.file("data","chgv_parameters.rda", package = "meadq")
## 
## burst_table_Plots(param.file=param.file, h5Files=h5Files)
## 


## ----PCA-start, eval=FALSE-----------------------------------------------
## filename.data =
## trt.params.wanted =
## output.folder =
## ctr.params.wanted =
## vars.wanted =
## 


## ----example-compute.ns, eval=F------------------------------------------
## example(compute.ns)


## ----correlation-index---------------------------------------------------
jay.data.file <- system.file("examples", "P9_CTRL_MY1_1A.txt",
                         package = "sjemea")
jay.s <- jay.read.spikes( jay.data.file)
plot.corr.index(jay.s)


## ----tiling-correlation--------------------------------------------------
data.file <- system.file("examples", "P9_CTRL_MY1_1A.txt",
                         package = "sjemea")
s <- jay.read.spikes(data.file)
t2 <- tiling.allpairwise(s)
require(lattice); levelplot(t2)


## ----eval=FALSE,include=TRUE---------------------------------------------
## require(knitr); knit2pdf('meadq-intro.Rnw')


