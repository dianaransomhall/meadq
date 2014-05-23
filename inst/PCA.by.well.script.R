# PCA.by.well.script.R
# author: Diana Hall
# 05/15/2014
# purpose: a script to run PCA analysis


#load necessary packages
library(sjemea)
library(FactoMineR)

#setwd for where output should appear
output.folder<- "F:/Kathleen/Analysis/PCA/"
trt.params.wanted=list(DIV.wanted=c("7","9"), trt.wanted=c('Acetaminophen'), dose.wanted=c(1,3,10) )
ctr.params.wanted = list(DIV.wanted=c("7","9"), 
                         trt.wanted=c('Acetaminophen'), 
                         dose.wanted=c(0) )

#variables possible: "dose", "meanfiringrate","burst.per.min", "mean.isis", "per.spikes.in.burst",
# "mean.dur","mean.IBIs", "nAE", "nABE","ns.n","ns.peak.m","ns.durn.m", "cv.network" ,                
# "ns.percent.of.spikes.in.ns", "ns.mean.insis","ns.durn.sd","ns.mean.spikes.in.ns","r"

vars.wanted = c( "dose", "meanfiringrate","burst.per.min", "mean.isis", "per.spikes.in.burst",
                 "mean.dur", "mean.IBIs", "nAE", "nABE", "ns.n", "ns.peak.m", "ns.durn.m" )  


PCA.by.well(filename.data = filename.data , trt.params.wanted = trt.params.wanted, 
              output.folder = output.folder, ctr.params.wanted = ctr.params.wanted,
              vars.wanted=NULL )


# alternatively the field may be assigned NULL and then a prompt will ask for parameter file

PCA.by.well(filename.data = NULL , trt.params.wanted = NULL, 
            output.folder = NULL, ctr.params.wanted = NULL,
            vars.wanted=NULL )


