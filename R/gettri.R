gettri<-function(mymat, lower=T){
  if(lower){
  num<- mymat[lower.tri(mymat) ]
  return(num)
  }else{
  num<- mymat[lower.tri(mymat) ]
  return(num)
  }
}
