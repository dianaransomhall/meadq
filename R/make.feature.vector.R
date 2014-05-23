make.feature.vector <-
function(s, feature, t.p ){
  
  if ( feature=="IBI" ){
    col.num=3
  } else if( feature =="durn" ){
    col.num=5
  }else if (feature == "mean.isis"){
    col.num = 6
  } else {
    print("No Feature selected: problem!")
  }
  
  
  #make a burst train
  feature.vector = list()
  feature.vector[[ length(s[[t.p]]$cw) ]]=0
  
  
  for (cur.ch in c(1:length( s[[t.p]]$cw) ) ){
    
    
    time= c(); feature = c() 
    if (length(s[[t.p]]$allb[[cur.ch]][,1] )>0){
      
      for (burst.n in c(1: length(s[[t.p]]$allb[[cur.ch]][,1] ) ) ){
        #this if statement is because the first row of s[[1]]$allb[[cur.ch]][1,3]==NA
        if ( ! (col.num==3 & burst.n==1 ) ){
          

        firstSoB = s[[t.p]]$allb[[cur.ch]][burst.n,1]
        time = c(time , s[[t.p]]$spikes[[cur.ch]][firstSoB] )
        feature=c(feature, s[[t.p]]$allb[[cur.ch]][burst.n, col.num])
        }
        
      }
      temp_3 = cbind (time, feature)
      
      feature.vector[[cur.ch]]= temp_3
      
    } else {
      feature.vector[[cur.ch]] = NA
      
    }
    
  } #end of loop through all channels
  
  feature.vector
}
