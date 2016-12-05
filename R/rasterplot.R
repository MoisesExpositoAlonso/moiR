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
rasterplot<-function(raster,objectname=deparse(substitute(raster)),name="",

  rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),
  midpoint=NULL,
  dimcol=50,
  numbreak=10,
  colbreaks=if(is.null(midpoint)){pretty(rangecol,dimcol) }else{ c(pretty(c(rangecol[1],midpoint ),n = round(dimcol/2)-1 ), midpoint,pretty(c(midpoint,rangecol[2] ),n = round(dimcol/2)-1 ) ) } %>% unique() ,
  legmid=NULL ,
  legbreaks=if(is.null(legmid)){pretty(rangecol,n = numbreak-1) }else{ c(pretty(c(rangecol[1],legmid ),n = round(numbreak/2) ), legmid,pretty(c(legmid,rangecol[2] ),n = round(numbreak/2) ) ) } %>% unique() ,
    vecol=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),

  contrast=0,
  pdf=F, add=F,plotlegend=T,discrete=F){

library(raster)

if(pdf == T ){ write.pdf(paste0("plots/raster_",objectname,"_",name,".pdf")) }

# Generate color
mypalette<-make.pallete.contrast(vecol)

# Generate sequence of breaks in legend

# Plot
if(discrete==T){
plot(raster, xaxt="n", yaxt="n", main="", col=vecol, axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)

}else{

plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(length(colbreaks)), breaks=colbreaks,axis.args=list(at=legbreaks),axes=F,box=F,frame.plot=F,bty="n")


}

title(paste(name))

if(pdf == T ){ dev.off() }

}
