calc.corr.matrix.by.well<-function (s, t.p, well, time.bin) 
{
  
  f = digi.spikes(s, time.bin=time.bin, t.p=t.p, w=well)
  elect.names <- names(f)
  f.mat<-matrix(unlist(f), ncol = length(elect.names), byrow = TRUE,
        dimnames=list(seq(1:length(f[[1]])), elect.names) )
  
  
  nelect = length(elect.names)
  r.matrix = matrix(-2, nrow=nelect, ncol=nelect,
        dimnames=list(elect.names, elect.names) ) 
  
  f.cor = cor(x = f.mat, y = NULL, use = "everything",
      method = c("pearson", "kendall", "spearman"))
  
  return(f.cor)

}