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
#' @examples
#' group1=rnorm(n = 100,mean = 1,sd = 1)
#' group1=rnorm(n = 100,mean = 2,sd = 1)
#' thetimenow<-contrast_test(group1,group2)
#'
#' @export
#' 

ggregression<-function(x,y,color="black",xlab="x",ylab="y"){
  #this produces a nice plot with some defaults. the x is first column and y the second

  library (ggplot2)
  toggplot2his=data.frame(x=as.numeric(x),y=as.numeric(y))

  ggplot(data=toggplot2his)+
    geom_point(aes(y=y,x=x),colour=transp(color))+
   geom_smooth(aes(y=y,x=x),method="glm",colour="darkred")+
    geom_smooth(aes(y=y,x=x),method="loess",colour="darkblue")+
    ylab(ylab)+
    xlab(xlab)
}

