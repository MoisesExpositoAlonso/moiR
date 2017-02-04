#' Quick scatter plot with points colored by a 3rd variable
#'
#' @param numeric1 a numeric vector of group 1
#' @param numeric2 a numeric vector of group 2
#' @param colorvar a numeric vector for the color gradient
#' @param breaks numeric value of how many braks are wanted for the color palette
#' @param pch what shape of points is wanted. default 19
#' @param alpha what transparency of dots is wanted
#'
#' @return A scatter plot from base plot with points colored by the rank of a given variable
#'
#' @examples
#' x=rnorm(n = 100,mean = 1,sd = 1)
#' y=rnorm(n = 100,mean = 2,sd = 1)
#' z=rnorm(n = 100,mean = 3,sd = 1)
#' plotwithcolors(x=x,y=y,colorvar=z)
#'
#' @seealso ggdotscolor
#'
#' @export
#'

plotdotscolor<-function(x,y,colorvar,palette=c("red","orange","yellow","green","blue"),breaks=50,pch=19,alpha=0.5,add=F,...){

colfun<-make.pallete.contrast(palette)
colores <- colfun(breaks) [as.numeric(cut(colorvar,breaks = breaks))]

if(add!=T){
  plot(x=x,y=y,cex=0)
}else{
  points(x=x,y=y,col=transparent(colores,alpha=alpha),pch=pch,...)
}
}

# plotwithcolors<-function(x,y,colorvar,palette=c("red","orange","yellow","green","blue"),pch=19,alpha=0.5){
#
#   colfun<-make.pallete.contrast(palette) #paleta de colores de azul(pocos trich) a rojo (muchos)
#   colores<-colfun(length(colorvar))[rank(colorvar)]
#
#   plot(x=x,y=y,col=transparent(colores,alpha=alpha),breaks=seq(range(colorvar,na.rm = T)[1],range(colorvar,na.rm = T)[2],10),pch=19)
# }
#
#
# accmaster$Col <- make.pallete.contrast( (brewer.pal(10,"RdBu")),raw = F )(breaks) [as.numeric(cut(log10(accmaster$T_repro),breaks = breaks))]
#
#
