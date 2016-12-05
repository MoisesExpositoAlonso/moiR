#' Pretty plot of raster object
#'
#' @param raster A raster layer object
#' @param objectname The name of the object plotted or other name to keep track of the pdf
#' @param name Title fo the plot
#' @param dimcol Number of color breaks in the gradient
#' @param vecol Color vector for the palette
#' @param breaks Breaks used in the color scale
#' @param rangecol Range of the color palette
#' @param numbreak Number of breaks in the color scale
#' @param contrast Interger for number of increase contrast iteration
#' @param pdf Logical whether to output plot as pedf
#' @param add Logical whether this plot will be overplotted on an existing one in the graphic device
#' @param plotlegend Logical whether the color legend is plotted. Useful when overlaying two maps.
#' @return Prints a plot of a raster object with a nice color palette and without axis (easily changed). If desired, it is outputed as pdf'.
#'
#' @examples
#'
#'
#'
#' @export
#'
envirplot<-function(raster,objectname=deparse(substitute(raster)),name="",
  dimcol=100,
  vecol=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),
  breaks=format(seq(rangecol[1],rangecol[2],len=numbreak),digits = 1),
  rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),
  numbreak=10,
  mysequence=NULL,
  midpoint=NULL,
  contrast=0,
  pdf=F, add=F,plotlegend=T,discrete=F){

library(raster)

if(pdf == T ){ write.pdf(paste0("plots/raster_",objectname,"_",name,".pdf")) }

# Generate color
mypalette<-make.pallete.contrast(vecol)

# Generate sequence of breaks in legend
if(is.null(mysequence)){ message('my sequence null, then creating sequence')
  if(is.null(midpoint)){  mysequence<- seq(rangecol[1],rangecol[2],len = numbreak-1)     }
    else
      {
  	  if( midpoint <rangecol[1] | midpoint > rangecol[2] ) { stop("Error: midpoint must be within the range of your data!") }
      mysequence<- c(seq(rangecol[1],midpoint,len = round(dimcol/2 )  ), seq(midpoint,rangecol[2],len = round(dimcol/2 )  ) [-1]  )
      }
}

# Plot
if(discrete==T){
plot(raster, xaxt="n", yaxt="n", main="", col=vecol, axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)

}else{

arg <- list(at=breaks)

print(mysequence)
print(breaks)
print(mypalette(dimcol))

# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,breaks=mysequence,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend) # this does not work anymore
plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend) # this does not work anymore
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), breaks=mysequence,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), breaks=mysequence,axis.args=arg,axes=F,box=F,frame.plot=F,bty="n")
# box()

}


title(paste(name))

if(pdf == T ){ dev.off() }

}
