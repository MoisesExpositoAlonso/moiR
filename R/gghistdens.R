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

gghistogram<-function(data,col="black",lab="x",density=F,shade=0){
  #this produces a nice plot with some defaults.
  # only one variable, it has to be a vector or a matrix one column
  #shade a %
  #if you choose density don't chose shade
  library (ggplot2)
  library(adegenet)
toggplot2hisdens <- with(density(data),data.frame(x,y))
toggplot2his<-data.frame(x=as.numeric(data))

if (shade>0)

perci<-as.numeric(quantile(toggplot2his$x,probs=shade/100))

ggplot()+
  geom_histogram(data=toggplot2his,aes(x=x,y = ..density..),
                 colour=transp("black"),fill=transp("grey"))+
  geom_line(data=toggplot2hisdens,aes(x=x,y=y),color="black") +
  geom_area(data = toggplot2hisdens,
            aes(x=ifelse(x <perci,x,perci), y=y),
            fill="red",alpha=.5)+
  ylim(c(0,max(toggplot2hisdens$y)+0.1*max(toggplot2hisdens$y)))+
  xlab(lab)


if (shade==0 && density==T)

    ggplot(data=toggplot2his,aes(x=x))+
    geom_histogram(aes(y = ..density..),colour=col)+
    geom_density(aes(y = ..density..),fill="grey",alpha=0.5)+
    xlab(lab)


if(shade==0 && density==F)
  ggplot(data=toggplot2his,aes(x=x))+
  geom_histogram(colour=col)+
  xlab(lab)

}

gghisdens<-function(data,col="black",lab="x"){
  #this produces a nice plot with some defaults.
  # only one variable, it has to be a vector or a matrix one column
  #shade a %
  #if you choose density don't chose shade
  library (ggplot2)
  library(adegenet)
  toggplot2hisdens <- with(density(data),data.frame(x,y))
  toggplot2his<-data.frame(x=as.numeric(data))


  ggplot(data=toggplot2his,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour=col)+
  geom_density(aes(y = ..density..),fill="grey",alpha=0.5)+
  xlab(lab)
}

gghisdensshade<-function(data,col="black",lab="x",shade=0){
  #this produces a nice plot with some defaults.
  # only one variable, it has to be a vector or a matrix one column
  #shade a %
  #if you choose density don't chose shade
  library (ggplot2)
  library(adegenet)
  toggplot2hisdens <- with(density(data),data.frame(x,y))
  toggplot2his<-data.frame(x=as.numeric(data))

  perci<-as.numeric(quantile(toggplot2his$x,probs=shade/100))

ggplot()+
  geom_histogram(data=toggplot2his,aes(x=x,y = ..density..),
                 colour=transp("black"),fill=transp("grey"))+
  geom_line(data=toggplot2hisdens,aes(x=x,y=y),color="black") +
  geom_area(data = toggplot2hisdens,
            aes(x=ifelse(x <perci,x,perci), y=y),
            fill="red",alpha=.5)+
  ylim(c(0,max(toggplot2hisdens$y)+0.1*max(toggplot2hisdens$y)))+
  xlab(lab)

}


myhistpropo<-function(x=x,xlab=NULL,col="black",border="white",main="",add=F){
  h =hist(x,xlab=xlab,col="black",border="white",main=main)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=F,add=add)
}

myhist<-function(x=x,xlab=NULL,col="black",border="white",main=""){
  hist(x,xlab=xlab,col="black",border="white",main=main )
}



myhist<-function(x=x,xlab=NULL,col="black",border="white",main="",breaks = "Sturges"){
  hist(x,xlab=xlab,col="black",border="white",main=main,breaks=breaks )
}


