#' Plot correlations of all pairs o variables .
#' 
#' @param dataset A numeric data frame 
#' @param columnpos In case you want to compute correlations of a subset of columns. Default NULL
#' @param filename Optional parameter, if provided a string, it will output the plot in a pdf in the working directory. Default NULL
#' @param the different methods possible in cor function. Default "pearson". Other possibilities are "spearman" or "kendall"
#'
#' @return If a file name is indicated, an output pdf plot. Otherwise, prints to the graphic device.
#'
#' @examples
#' library(MASS)
#' data(mtcars)
#' cormatrix_r_p(as.numeric(na.omit(mtcars)))
#'
#' @export
#'

plot_corrplot<-function(dataset,columnpos=NULL,filename=NULL, method="pearson"){
require(corrplot)

if(is.null(columnpos)){
	columnpos=1:dim(dataset)
}

tocorrelate<-dataset[,columnpos]
tocorrelate.numeric<-na.omit(apply(tocorrelate,2,as.numeric) )

# correlate
c=cor(tocorrelate.numeric,use="pairwise.complete.obs",method=method)

# output pdf
if(!is.null(filename)){
pdf(paste(filename,".pdf",sep=""),height = 15,width = 15)
}

corrplot.mixed(c,tl.pos="lt")
# corrplot.mixed(c,tl.pos="lt",col=my.col)

if(!is.null(filename)){dev.off() }
 
}