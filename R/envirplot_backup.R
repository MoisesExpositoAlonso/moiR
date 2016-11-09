envirplot<-function(raster,objectname=deparse(substitute(raster)),name="",dimcol=100,vecol=c('cyan','dodgerblue1','blue','forestgreen','orange','red','purple'),
  breaks=format(seq(rangecol[1],rangecol[2],len=numbreak),digits = 1),
  rangecol=c(min(values(raster),na.rm = T),max(values(raster),na.rm = T) ),numbreak=5,contrast=0,pdf=F, add=F,plotlegend=T){
library(raster)

if(pdf == T ){ write.pdf(paste0("plots/raster_",objectname,"_",name,".pdf")) }

# Generate color
mypalette<-make.pallete.contrast(vecol)

# Generate sequence of breaks in legend 
mysequence<- seq(rangecol[1],rangecol[2],len = dimcol-1)
arg <- list(at=breaks)

# Plot 
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)
plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), axis.args=arg,breaks=mysequence,axes=F,box=F,frame.plot=F,bty="n",add=add,legend=plotlegend)
# plot(raster, xaxt="n", yaxt="n", main="", col=mypalette(dimcol), breaks=mysequence,axis.args=arg,axes=F,box=F,frame.plot=F,bty="n")
# box()
# axis(1,las=1)
# axis(2,las=1)
title(paste(name))

if(pdf == T ){ dev.off() }

}
