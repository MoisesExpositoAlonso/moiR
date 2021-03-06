#' A customized pdf function
#' 
#' @param file filename. Include .pdf ending
#' @param height height of the plot
#' @param width width of the plot
#' @param metricsystem String either "mm" or "inch". Default "mm" as it should be ;P
#' @return outputs a pdf
#' @examples
#' \dontrun{
#' 
#' write.pdf(filename="myplot",heightmm=173,width=85)
#' plot(1,1)
#' dev.off()	
#' }
#' @export

write.pdf<-function(filename,heightmm=173,widthmm=173,metricsystem="mm",timestamp=F){
  if(metricsystem !='inch'){
  i2mm<-25.4
	heightmm=heightmm/i2mm
	widthmm=widthmm/i2mm
  }

  if(timestamp==T){ substrRight(filename,lastpos=4,	giveright=F) }
  
  pdf(useDingbats=F,file=filename,height=heightmm,width=widthmm)
}