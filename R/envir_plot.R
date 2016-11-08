#' Pretty plot of raster object
#' 
#' @parameter raster A raster layer object
#' @parameter objectname a numeric vector of group 2
#' @parameter name 
#' @parameter dimcol
#' @parameter vecol
#' @parameter rangecol
#' @parameter pdf
#' @parameter numbreak
#' @parameter contrast
#' @parameter method string, either t.student (parametric) or wilcox.test (nonparametric)
#' @parameter title string with the name of the plot
#' @parameter wantpoints Logical FALSE/TRUE to determine whether points will be overplotted
#' @parameter wantviolin Logical FALSE/TRUE to determine whether you want a violine plot in the background
#' @parameter The colors of the two groups. Default c("darkorchid4","aquamarine4")
#'
#' @return Prints a plot with the given test p-value. If title is provided, it prints into a pdf.
#' @examples
#' group1=rnorm(n = 100,mean = 1,sd = 1)
#' group1=rnorm(n = 100,mean = 2,sd = 1)
#' thetimenow<-contrast_test(group1,group2)
#' @export
#' 
envir_plot<-function(raster,objectname=deparse(substitute(raster)),dimcol=100,name="",vecol=NULL,breaks=format(seq(rangecol[1],rangecol[2],len=numbreak),digits = 1)
,rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),numbreak=5,contrast=0,pdf=F){
library(raster)

if(pdf == T ){ write.pdf(paste0("plots/raster_",objectname,"_",name,".pdf")) }

# Generate color
if( is.null(vecol ) ){
mypalette<-make.pallete.contrast(c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),contrast)
}else{
mypalette<-make.pallete.contrast(vecol)
}

# Generate sequence of breaks in legend 
arg <- list(at=breaks)

# Plot 
plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,axes=F,box=F,frame.plot=F,bty="n")
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), breaks=mysequence,axis.args=arg,axes=F,box=F,frame.plot=F,bty="n")
# box()
# axis(1,las=1)
# axis(2,las=1)
title(paste(name))

if(pdf == T ){ dev.off() }

}
