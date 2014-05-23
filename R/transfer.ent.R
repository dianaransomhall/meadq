transfer.ent <-
function(Y,X,lag=1){
  
  L4=L1=length(X)-lag
  L3=L2=length(X)
  ##########################
  #1 p(Xn+s, Xn, Yn)
  ##########################
  TPvector1=rep(0,L1)
  for(i in 1:L1){
    TPvector1[i] = paste(c(X[i+lag], "i", X[i], "i",Y[i]), collapse="" )
  }
  
  TPvector1T=table(TPvector1)/length(TPvector1)
  
  ###############
  #2. p(Xn)
  ###############
  TPvector2=X
  TPvector2T=table(X)/sum(table(X))
  ############
  # 3. p(Xn,Yn)
  ###############
  TPvector3=rep(0,L3)
  for(i in 1:L3){
    TPvector3[i]=paste(c(X[i],"i",Y[i]), collapse="")
    
  }
  TPvector3T=table(TPvector3)/length(TPvector2)
  
  # 4 p(Xn+s, Xn)
  TPvector4=rep(0,L4)
  for(i in 1:L4){
    TPvector4[i]=paste(c(X[i+lag],"i",X[i]),collapse="")
  }
  
  TPvector4T=table(TPvector4)/length(TPvector4)
  
  #++++++++++++++++++++++++++
  # transfer entropy T(Y->X)
  #+++++++++++++++++++++++++++++
  SUMvector=rep(0,length(TPvector1T) )
  
  for(n in 1:length(TPvector1T) )
  {
    SUMvector[n] = TPvector1T[n] * log10((TPvector1T[n] * TPvector2T[(unlist(strsplit(names(TPvector1T)[n],
                                                                                      "i")))[2] ])/(TPvector3T[paste((unlist(strsplit(names(TPvector1T)[n],"i")))[2],"i",(
                                                                                        unlist(strsplit(names(TPvector1T)[n],"i")))[3],  sep="",  collapse="") ] * TPvector4T
                                                                                        [paste((unlist(strsplit(names(TPvector1T)[n],"i")))[1], "i", (unlist(strsplit(names(
                                                                                          TPvector1T)[n], "i" )))[2], sep="", collapse="")]))
    
  }
  return( sum(SUMvector) )
  
  
}
