#' Correlations of all pairs o variables.
#' 
#' @param numericdatanona A numeric data frame 
#' @param columnpos In case you want to compute correlations of a subset of columns. Default NULL
#' @param filename Optional parameter, if provided a string, it will output the plot in a pdf in the working directory. Default NULL
#' @param method the methods inherited from rcorr. Default "pearson". Other possibility is "spearman".
#'
#' @return If a file name is indicated, an output pdf plot. Otherwise, prints to the graphic device.
#' @return A matrix with lower triangle R pearson correlations, and upper triangle the p-values
#'
#' @examples
#' library(MASS)
#' data(mtcars)
#' cormatrix_r_p(as.numeric(na.omit(mtcars)))
#'
#' @export
#'
cormatrix_r_p<-function(numericdatanona,columnpos=NULL,filename=NULL,method="pearson") {

if(is.null(columnpos)){ 	columnpos=1:dim(dataset) }

# check that dataset is good
numericdatanona<-numericdatanona[,columnpos]
numericdatanona<-na.omit(apply(numericdatanona,2,as.numeric) )


# get correlations
requie("Hmisc")
rcorr(numericdatanona)
rp<-rcorr(numericdatanona,method=method)
attributes(rp)
rp$r
rp$r[ lower.tri(rp$r) ]
dummy<-rp$r
dummy[ lower.tri(dummy) ] = rp$P[lower.tri(rp$P) ]

# perhaps write
if(!is.null(filename)){
write.csv(dummy,file =paste(filename,".csv",sep=""))
}

return(dummy)

}