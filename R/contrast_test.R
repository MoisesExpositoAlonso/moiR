#' Quick plot + contrast hypothesis test to compare two populations
#' 
#' @parameter numeric1 a numeric vector of group 1 
#' @parameter numeric2 a numeric vector of group 2
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

contrast_test<-function(numeric1,numeric2,method ="t.test",title=NULL,wantpoints=FALSE,wantviolin=TRUE,colorpair=c("darkorchid4","aquamarine4")){

numeric1name=deparse(substitute(numeric1))
numeric2name=deparse(substitute(numeric2))

# decide whith test
if(method =="t.test"){  mytest<-t.test(numeric1,numeric2) }
else if (method=="wilcox.test") { mytest<-wilcox.test(numeric1,numeric2)  }
else{print("need to provide appropriate test: t.test or wilcox.test")}
p<-format(mytest$p.value,digits=3)

#print test
print(mytest)

# go for plot
if(!is.null(title)){
write.pdf(paste("contrast_plot",title,numeric1name,numeric2name,sep="_"))
}

      # type boxplot
      if(wantviolin ==F ){
      boxplot(numeric1,numeric2,col=transparent(colorpair),borders="white", names=c(numeric1name,numeric2name))
      mtext(paste(method,"p = ",p ),side = 1)
      # text(x=1.5,y=max(c(numeric1,numeric2)),labels = format(p,digits=4) )
        if(wantpoints==T ){
        points(y= numeric1, pch=19,col=transparent("black",alpha = 0.2) , x=jitter(rep(1,length(numeric1)),factor = 2))
        points(y= numeric2, pch=19,col=transparent("black",alpha = 0.2) , x=jitter(rep(2,length(numeric2)),factor=2))
        }}

      # type violin
      else if(wantviolin==T){
      library(ggplot2)
      library(cowplot)
      numeric1data<-data.frame(variable=numeric1name,value=numeric1)
      numeric2data<-data.frame(variable=numeric2name,value=numeric2)
      toplot_melt<-rbind(numeric1data,numeric2data)

      p<-ggplot(data=toplot_melt)+geom_violin(trim=FALSE,aes(x = variable,y=value,fill=variable),alpha=0.2)+ theme_cowplot()+xlab("")+ylab("")+
       scale_fill_manual(values =colorpair,guide=F )+geom_boxplot(aes(x = variable,y=value ),color=colorpair,width=0.2)+ggtitle(paste(method,"p = ",p )) 
        if(wantpoints==T ){
        p<-p + geom_point(aes(x = variable,y=value,color=variable))+scale_color_manual(values=transparent(colorpair))
        print(p)
        }else{print(p)}
      }

# print if required
if(!is.null(title)){  dev.off()  }

}

