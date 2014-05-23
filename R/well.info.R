well.info <-
function(s2) {
  if ("12"==substring(s2$layout$array,7,8)){
    s2$wells<-c("A1","A2","A3","A4","B1","B2","B3","B4","C1","C2","C3","C4")
  } else if ("48"==substring(s2$layout$array,7,8)){
    s2$wells<-c('A1','A2','A3','A4','A5','A6','A7','A8',
                'B1','B2','B3','B4','B5','B6','B7','B8',
                'C1','C2','C3','C4','C5','C6','C7','C8',
                'D1','D2','D3','D4','D5','D6','D7','D8',
                'E1','E2','E3','E4','E5','E6','E7','E8',
                'F1','F2','F3','F4','F5','F6','F7','F8')
  }
  #add number of active electrodes
  s2$nAE<-rep(0,length(s2$wells))
  names(s2$nAE)<-s2$wells
  for (i in 1:s2$NCells){
    s2$nAE[which(substr(s2$channels[i],1,2)==(s2$wells))]=
      s2$nAE[which(substr(s2$channels[i],1,2)==(s2$wells))]+1
    s2$cw[i]<-substr(s2$channels[i],1,2)
  }
   
  s2
}
