mypalettes<-function(name){
     
  mypalettes<-list(
     colors11=c("#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","gold","#a6cee3") ,
     jet.colors()
                   )

  if(is.null(name)){ return(mypalettes)}
  else{ return(mypalettes[[name]] ) }
}
 

