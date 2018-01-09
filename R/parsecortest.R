parsecortest<-function(mod){
  r=mod$estimate
  p=mod$p.value
  paste0("r= ",round(r,digits = 2), ", p=",format(p,digits=2,scientific = TRUE))
}

