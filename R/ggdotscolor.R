#' Quick scatter plot with points colored by a 3rd variable
#' 
#' @parameter x a numeric vector of group 1 
#' @parameter y a numeric vector of group 2
#' @parameter varcol either a numeric vector (continuous scale plotting) or factor variable (discrete plotting)
#' @parameter data A dataset with 3 columns. It is assumed that 1 is x, 2 is y, 3 is varcol. This substitutes the use of x, y and varcol.
#' @return A scatter plot from base plot with points colored by the rank of a given variable
#' @examples
#' x=rnomr(1,1,100)
#' y=rnorm(1,2,100)
#' x=rnorm(1,3,100)
#' plotwithcolors(x=x,y=y,colorvar=z)
#' 
#' @seealso ggplot_world_map
#' @export
#' 

ggdotscolor<-function(data=NULL,x,y,varcol, ylab=NULL,xlab=NULL,collab=NULL,mycolors=brewer.pal(10,name = "RdBu")){
library(ggplot2);library(cowplot);library(RColorBrewer)

# data frame
if(!is.null(data)){
x=data[,1]
y=data[,2]
varcol=data[,3]
}else{}

toplot<-data.frame(x,y,varcol)

#make base plot
if(is.null(ylab)){ xlab=deparse(substitute(x)) }
if(is.null(xlab)){ ylab=deparse(substitute(y)) }
if(is.null(collab)){ collab=deparse(substitute(varcol)) }

p<-ggplot(toplot)+geom_point(aes(x=x,y=y , col=varcol ),alpha=0.5) + xlab(xlab)+ylab(ylab)

# color 
if(is.factor(toplot$varcol)){
	print("varcol is a factor!")
		if(mycolors==brewer.pal(10,name = "RdBu")){
		mycolors=c("#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","gold","#a6cee3")
		}
	colpal<-make.pallete.contrast(colors11)
	p<-p+ scale_color_manual(collab,values=colpal(length(levels(varcol))),breaks=levels(varcol) )

}else{
	# print("varcol is a numeric!")
	mycolors=mycolors
	# colpal<-make.pallete.contrast(mycolors)

	p<- p+  scale_colour_gradientn(collab,colours=mycolors)

}
return(p)
}