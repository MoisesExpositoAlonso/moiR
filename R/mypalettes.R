mypalettes<-function(name=NULL,rsample=F){

  mypalettes<-list(
     colors11=c("#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","gold","#a6cee3") ,
     jet=jet.colors(),
     jetb=c("black",jet.colors()) [-c(2,10)],
     saturated=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),
     saturated2=c(   "navy" ,  "darkblue"   , "#0074d9" ,   "#7fdbff"  ,  "#39cccc" ,   "#3d9970" ,   "#ffdc00"   , "#ff851b"  ,  "#ff4136"  ,  "#85144b"  ,  "#7442c8" ),
     moispectral = c(c("black",rev(brewer.pal(n = 10,name = "Spectral"))[-1]))[-10]
     # moispectral = c(c("black","#42dff4",rev(brewer.pal(n = 10,name = "Spectral"))[-2]))
  )
  if(is.null(name)){ return(mypalettes)}
  else{ return(mypalettes[[name]] ) }
}


.display_pallete <- function(name){
  
  thecolors <- mypalettes(name)
  
  plot(x=1:length(thecolors),y=rep(1,length(thecolors)),
       col=thecolors,
    cex=3,pch=19,
    ylim=c(0,length(thecolors)),
    axes = FALSE, bty = "n", xlab = "", ylab = ""
    )
  
    text( x =1:length(thecolors),y=0,
          labels=as.character(1:length(thecolors)))
  
  }
# .display_pallete("moispectral")
# .display_pallete("jetb")
