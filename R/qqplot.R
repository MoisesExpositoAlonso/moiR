#' Produce a qqplot to evaluate p-value inflation.
#' 
#' @param pvals A numeric vectors with p-values 
#' @param  name If outputing plot is desirable, string.
#' @return Either outputs a plot in pdf or prints to the graphic device
#' @examples
#' qqplot(pvals=c(0.001,0.05,0.9))
#'



qqplot<-function(pvals,name=NULL){

  observed <- sort(pvals)
  lobs <- -(log10(observed))

  expected <- c(1:length(observed))
  lexp <- -(log10(expected / (length(expected)+1)))

  if(!is.null(name)){
    pdf(paste("qqplot",name,".pdf",sep=""), width=6, height=6)
  }

  plot(c(0,7), c(0,7), col="red", lwd=3, type="l", xlab="Expected (-logP)", ylab="Observed (-logP)", xlim=c(0,max(lobs)+1), ylim=c(0,max(lobs)+1), las=1, xaxs="i", yaxs="i", bty="l")
  points(lexp, lobs, pch=23, cex=.4, bg="black")
  abline(lm(lobs~lexp ),col="black",lty="dashed")
  title( paste("lambda",round(coefficients(lm(lobs~lexp ))[2],digits=3) ) )
  
  if(!is.null(name)){
    dev.off()
  }
}
 
 qqGWA<-qqplot