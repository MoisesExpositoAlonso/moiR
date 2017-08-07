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

# envirplot(raster)
envirplot<-function(raster,
                    objectname=deparse(substitute(raster)),
                    name="",
                    midpoint=NULL,
                    rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),
                    dimcol=100,
                    numbreak=10,
                    legendbreak = format( if(!is.null(midpoint)){c(seq(rangecol[1],midpoint,len = round(numbreak/2 )  ), seq(midpoint,rangecol[2],len = round(numbreak/2 )  ) [-1]  )}else{seq(rangecol[1],rangecol[2],len=numbreak)} , digits=1),
                    colorbreak = if(!is.null(midpoint)){c(seq(rangecol[1],midpoint,len = round(dimcol/2 )  ), seq(midpoint,rangecol[2],len = round(dimcol/2 )  ) [-1]  )}else{seq(rangecol[1],rangecol[2],len=dimcol-1)},
                    vecol=NULL,
                    contrast=1,
                    discretebreaks=na.omit(unique(values(raster))),
                    discretelabels=na.omit(unique(values(raster))),
                    pdf=F, add=F,plotlegend=T,discrete=F,
                    ...){

if(length(vecol)==1 & !is.null(vecol)){ discrete=T}

colorbreak=unique(colorbreak)

if(discrete==F){
  if(is.null(vecol)){vecol=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple')}
  mypalette=make.pallete.contrast(vecol,contrast)
  raster::plot(raster,xaxt="n", yaxt="n",ylab="",xlab="" ,main=name ,breaks=colorbreak,axis.args=list(at=legendbreak), col=mypalette(dimcol-2), axes=F,box=F,frame.plot=F,bty="n", add=add, legend=plotlegend,...)

}else{

  if(is.null(vecol)){ vecol= mypalettes("colors11")[1:length(discretelabels)] }
  raster::plot(raster,xaxt="n", yaxt="n",ylab="",xlab="" ,main=name ,axis.args=list(at=discretebreaks,label=discretelabels), col=vecol, axes=F,box=F,frame.plot=F,bty="n", add=add, legend=plotlegend,...)

}

}


