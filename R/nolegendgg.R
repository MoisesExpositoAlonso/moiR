#' Remove the legend of an existing ggplot
#'
#' @param plot
#'
#' @export
nolegendgg<-function(plot){

  return(plot + theme(legend.position="none"))
}
