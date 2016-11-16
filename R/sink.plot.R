#' Print a plot stored in an active binding or object into a pdf.
#' 
#' @param plot An active binding or an object of a plot
#' @param  name Name of the pdf where the plot is printd. Default, activebinding name.
#'
#' @return Either outputs a plot in pdf or prints to the graphic device
#'
#' @examples
#' p<-plot(1,1)
#'
#' @export
sink.plot<-function(plot,name=deparse(substitute(plot))){
	print(paste('printing plot to ...', paste0(getwd(),'/',name,".pdf")))
	write.pdf(paste0(name,".pdf"))
	plot
	dev.off()
}