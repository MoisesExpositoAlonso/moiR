mypalettes<-function(name=NULL){

  mypalettes<-list(
     colors11=c("#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","gold","#a6cee3") ,
     jet=jet.colors(),
     saturated=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),
     saturated2=c(   "navy" ,  "darkblue"   , "#0074d9" ,   "#7fdbff"  ,  "#39cccc" ,   "#3d9970" ,   "#ffdc00"   , "#ff851b"  ,  "#ff4136"  ,  "#85144b"  ,  "#7442c8" )
                   )

  if(is.null(name)){ return(mypalettes)}
  else{ return(mypalettes[[name]] ) }
}


