#' Color palete jet colors from Matlab
#' 
#' @param palette Logical whether you want to return a palette object or not.
#' @return Either a vector of colors or a palette object
#'
#' examples
#' plot(1:10,1:10, col=jet.colors(palette=T))
#' @export

jet.colors<-function(palette=F){
jet.colors <-  c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
if(palette==T){
  return(colorRampPalette(jet.colors))
}else{return(jet.colors)}
}