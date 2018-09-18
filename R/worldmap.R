#' Create a worldmap
#'
#' @param mapcol color string of the color of the maps
#' @param backcol color string of the background of the plot
#' @param countrycol color string of the lines separating countries
#' @param xlim
#' @param ylim
#'
#' @return
#' returns a ggplot object
#'
#' @export
#'
#' @examples
#' p<-ggplot_world_map()
#' p<-geom_point(data=data.frame(lat=41, lon=7), aes(x=lon, y=lat))
#'

ggplot_world_map<-function(mapcol="seashell3",backcol="white",countrycol=mapcol,projection="cartesian",xlim=c(-200,+200),ylim=c(-60,+80),orientation=c(80,0,0)){

stopifnot(projection %in% c("cartesian","perspective","mercator"))
library(ggplot2)
library(cshapes)
library(gpclib)
gpclibPermit() # there is a problem in one package if you don't allow this

world <- cshp(date=as.Date("2008-1-1"))
world.points <- fortify(world, region='COWCODE')

xticks=round( seq(xlim[1],xlim[2],length.out = 10 ),digits=0)
yticks=round( seq(ylim[1],ylim[2],length.out = 10 ),digits=0)

p1<-ggplot()
p1<-p1+ geom_polygon(data=world.points, aes(long,lat,group=group) ,fill=mapcol,col=countrycol)

if(projection=="cartesian") {p1<-p1+   coord_cartesian(xlim = xlim,ylim=ylim)
}else if(projection=="mercator"){p1<-p1+   coord_map("mercator",xlim = xlim,ylim=ylim)
}else if(projection=="perspective"){p1<-p1+   coord_map("perspective",dist=1, xlim = xlim,ylim=ylim)
}else if(projection=="ortho"){p1<-p1+   coord_map("ortho", xlim = xlim,ylim=ylim, orientation=orientation)}
else{stop("Unknown projection")}

p1<-p1+ xlab("")+ylab("")
p1<-p1+ scale_x_continuous(breaks=xticks, labels=paste(abs(xticks), ifelse(xticks<0,"W","E" ) ,sep=" ") )
p1<-p1+ scale_y_continuous(breaks=yticks ,labels= paste(yticks ,"N",sep=" ") )

p1<-p1+ theme(panel.grid.major = element_line(colour = "white",size=0,linetype="dashed"))
p1<-p1+ theme(panel.grid.minor = element_line(colour = "white",size=0,linetype="dashed"))
# p1<-p1+ theme(panel.border=element_rect(colour="black",fill=NA))

p1 <- p1+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = backcol, colour = 'white'),
                panel.border=element_rect(colour="black",fill=NA),legend.title = element_blank(),legend.background=element_blank(),legend.key = element_blank())
p1

return(p1)
}
