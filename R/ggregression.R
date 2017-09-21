#' Quick plot + contrast hypothesis test to compare two populations
#'
#' @param numeric1 a numeric vector of group 1
#' @param numeric2 a numeric vector of group 2
#' @param method string, either t.student (parametric) or wilcox.test (nonparametric)
#' @param title string with the name of the plot
#' @param wantpoints Logical FALSE/TRUE to determine whether points will be overplotted
#' @param wantviolin Logical FALSE/TRUE to determine whether you want a violine plot in the background
#' @param The colors of the two groups. Default c("darkorchid4","aquamarine4")
#'
#' @return Prints a plot with the given test p-value. If title is provided, it prints into a pdf.
#'
#' @seealso ggdotscolor
#'
#' @examples
#' x=rnomr(1,1,100)
#' y=rnorm(1,2,100)
#' x=rnorm(1,3,100)
#' ggdotscolor(x=x,y=y,colorvar=z) %>% addggregression()
#'

ggregression<-function(x,y,color="black",xlab=deparse(substitute(x)),ylab=deparse(substitute(y)), doregression=T,doloess=F,span=if(doloess==T){ c(.5,1,1.5,2)},...){
#this produces a nice plot with some defaults. the x is first column and y the second
library(ggplot2)
library(cowplot)
tp=data.frame(x=as.numeric(x),y=as.numeric(y))
myplot<- ggplot(data=tp,aes(y=y,x=x),colour=color, ...)+geom_point() +    ylab(ylab)+     xlab(xlab)
# p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)
if(doregression==T){
myplot<- myplot+ geom_smooth(aes(y=y,x=x),method="glm",colour=color)+   annotate("text",  x=Inf, y = Inf, label = lm_eq(y,x),parse=FALSE, vjust=1, hjust=1)
}
if(doloess==T){
for(span in span){
myplot<-  myplot + geom_smooth(aes(y=y,x=x),method="loess",colour=transparent(color),span=span,se=F)
}
}
return(myplot)
}
####
# ggregression<-function(...){
#   args<-list(...)
#   ggdotscolor(...) %>% addggregression(. ,args)
#
#
# }
