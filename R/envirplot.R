#' Pretty plot of raster object
#' 
#' @parameter raster A raster layer object
#' @parameter objectname The name of the object plotted or other name to keep track of the pdf
#' @parameter name Title fo the plot
#' @parameter dimcol Number of color breaks in the gradient
#' @parameter vecol Color vector for the palette
#' @parameter breaks Breaks used in the color scale
#' @parameter rangecol Range of the color palette
#' @parameter numbreak Number of breaks in the color scale
#' @parameter contrast Interger for number of increase contrast iteration
#' @parameter pdf Logical whether to output plot as pedf
#' @parameter add Logical whether this plot will be overplotted on an existing one in the graphic device
#' @parameter plotlegend Logical whether the color legend is plotted. Useful when overlaying two maps.
#' @return Prints a plot of a raster object with a nice color palette and without axis (easily changed). If desired, it is outputed as pdf'.
#'
#' @examples
#' 
#' 
#' 
#' @export
#' 
envirplot<-function(raster,objectname=deparse(substitute(raster)),name="",dimcol=100,vecol=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),
  breaks=format(seq(rangecol[1],rangecol[2],len=numbreak),digits = 1),
  rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),numbreak=5,mysequence=NULL,midpoint=NULL,contrast=0,pdf=F, add=F,plotlegend=T,discrete=F){
library(raster)

if(pdf == T ){ write.pdf(paste0("plots/raster_",objectname,"_",name,".pdf")) }

# Generate color
mypalette<-make.pallete.contrast(vecol)

# Generate sequence of breaks in legend 
if(!is.null(mysequence)){
if(is.null(midpoint)){
mysequence<- seq(rangecol[1],rangecol[2],len = dimcol-1)
}
else{
	if( midpoint <rangecol[1] | midpoint > rangecol[2] ) { stop("Error: midpoint must be within the range of your data!")
	}
mysequence<- c(seq(rangecol[1],midpoint,len = round(dimcol/2 )  ), seq(midpoint,rangecol[2],len = round(dimcol/2 )  ) [-1]  )
}
}
arg <- list(at=breaks)

# Plot 
if(discrete==T){
plot(raster, xaxt="n", yaxt="n", main="", col=vecol, axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)

}else{
plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,breaks=mysequence,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)

}
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), breaks=mysequence,axis.args=arg,axes=F,box=F,frame.plot=F,bty="n")
# box()
# axis(1,las=1)
# axis(2,las=1)
title(paste(name))

if(pdf == T ){ dev.off() }

}
