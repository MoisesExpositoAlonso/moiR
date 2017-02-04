#' Quick scatter plot with points colored by a 3rd variable
#'
#' @param x a numeric vector of group 1
#' @param y a numeric vector of group 2
#' @param varcol either a numeric vector (continuous scale plotting) or factor variable (discrete plotting)
#' @param data A dataset with 3 columns. It is assumed that 1 is x, 2 is y, 3 is varcol. This substitutes the use of x, y and varcol.
#'
#' @return A scatter plot from base plot with points colored by the rank of a given variable
#'
#' @examples
#' x=rnomr(1,1,100)
#' y=rnorm(1,2,100)
#' x=rnorm(1,3,100)
#' ggdotscolor(x=x,y=y,colorvar=z)
#'
#' @seealso ggplot_world_map
#' @export
#'

ggdotscolor<-function(x,y,varcol=NULL, ylab=deparse(substitute(y)),xlab=deparse(substitute(x)),collab=deparse(substitute(varcol)),mycolors=brewer.pal(10,name = "RdBu"),contrast=0,...){

if(is.null(varcol)){
  varcol<-rep(1,length(x))
  mycolors<-c("black")
  collab=NULL

}


toplot<-data.frame(x,y,varcol)

#make base plot

p<-ggplot(toplot,aes(x=x,y=y , colour=varcol ),...)+geom_point(...) + xlab(xlab)+ylab(ylab)

# color
if(is.factor(toplot$varcol)){
	print("varcol is a factor!")
		if(mycolors==brewer.pal(10,name = "RdBu")){
		mycolors=c("#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","gold","#a6cee3")
		}
	colpal<-make.pallete.contrast(mycolors,contrast)
	p<-p+ scale_color_manual(collab,values=colpal(length(levels(varcol))),breaks=levels(varcol) )

}else{
	# print("varcol is a numeric!")
	mycolors=mycolors
	# colpal<-make.pallete.contrast(mycolors)

	p<- p+  scale_colour_gradientn(collab,colours=mycolors)

}

if(is.null(collab) ){ return( p %>% nolegendgg())}
else{return(p)}

}
