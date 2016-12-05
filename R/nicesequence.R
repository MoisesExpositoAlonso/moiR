# #' Pretty plot of raster object
# #'
# #' @param breaks Breaks used in the color scale
# #' @param rangecol Range of the color palette
# #' @param numbreak Number of breaks in the color scale
# #'
# #' @examples
# #'
# #'
# #'
# #' @export
# #'
#
# nicesequence<-function(breaks,rangecol,){}
#
# x=rnorm(100,mean = 0.006,0.0005)
# num=10
#
# library(classInt)
# myinterval=classIntervals(x,n=num,style="pretty",between=c(min(x),mean(x)), cutlabels=T)
#
# myinterval$brks
#
# classIntervals(x,n=num,style="fixed",dataPrecision = )
#
#
#
#  breaks=format(seq(rangecol[1],rangecol[2],len=numbreak),digits = 1),
#   rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),
#   numbreak=10,
#   mysequence=NULL,
#   midpoint=NULL,
