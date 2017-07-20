findroot<-function(file){
  rootfile<-tail( strsplit(file,split = '/')[[1]] ,n = 1)
  return(rootfile)
}
