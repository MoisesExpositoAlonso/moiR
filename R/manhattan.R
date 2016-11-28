#' Quick scatter plot with points colored by a 3rd variable
#'
#' @param data A data.frame or matrix, with colnames 'chr', 'pos', and 'stat'.
#' @param chrpalette A vector of colors or single color
#' @param type Wether you want each chromosome separatedly plotted or all together. String "cumulative" or "perchromosome". Default cumulative
#' @param nameplot	data A dataset with 3 columns. It is assumed that 1 is x, 2 is y, 3 is varcol. This substitutes the use of x, y and varcol.
#' @param empirical Logical. Is the p value empirical p-value (that is, is in the range of < 0.0001?). Default FALSE.
#' @param pdfname	pdf name to export plot
#' @param res	Numeric with the resoution to which chromosome positions wants to be round. Default Megabase.
#' @param ifpvalue Logical, flag to FALSE when using effect sizes instead of p-values
#' @param ylab Name of y lab. Default log10 p-value or statistic.
#' @param everymb Resolution of the axis scale. Default one label every 1 Mb.
#'
#' @return A scatter plot from base plot with points colored by the rank of a given variable
#'
#' @export
#'
manhattan<-function(data,
	chrpalette=c('black','black','black','black',"black"),
	type='cumulative',
	everymb=1,
	nameplot='',pdfname=if(nameplot!=""){ paste0(nameplot, 'manhattan',".pdf")}else{NULL},
	res=1e6,
	empirical=if(ispvalue==TRUE){FALSE}else{"no"},
	ispvalue=TRUE,
	ylab=if(ispvalue==TRUE){ylab="log10 p-values"}else{"statistic"}
	)
{

# define parameters
if(ispvalue==T){ data$stat = -log10(data$stat)}
numchr=length(unique(data$chr))
sizes=if(ispvalue==T){(data$stat)/max((data$stat))}else{ (abs(data$stat)-min(abs(data$stat))) / (max(abs(data$stat))-min(abs(data$stat))) }
# chrpalette<-if(length(chrpalette) != numchr ){chrpalette<-rep(chrpalette[1],numchr)}
# sizes=1


# plot 1
if(type =='perchromosome'){
print(paste0('to plot a manhattan in a single plot, use flag type cumulative'))

if(!is.null(pdfname)){  write.pdf(file = pdfname,height = 40*numchr ); print(paste0('printing manhattan plot to ...', pdfname)) }
par(mfrow=c( numchr,1),mar = c(4,4,1.5,1))

for (chromnum in sort(unique(data$chr) )){

subdata<-subset(data,data$chr==chromnum)
subdata$chr<-as.numeric(subdata$chr)
maxchrompos<-max(data$pos)
#chcol<-c('firebrick3','lightskyblue','palegreen3','orchid4',"tan2")data
sizes=if(ispvalue==T){(subdata$stat)/max((subdata$stat))}else{ (abs(subdata$stat)-min(abs(subdata$stat))) / (max(abs(subdata$stat))-min(abs(subdata$stat))) }



plot(y=(subdata$stat), x= (subdata$pos / 1e6) ,pch=19,cex=sizes,col=chrpalette[subdata$chr],xlab="Position (Mb)",  ylab= ylab ,xaxt="n",xlim=c(0,maxchrompos/1e6),frame.plot = F)

if(empirical==TRUE ){
  abline(h=(0.05),col="black",lty=2)
  abline(h=(0.01),col="black",lty=2)
  abline(h=(0.001),col="black",lty=2)
  }
else if(empirical==FALSE){
	abline(h=(0.05/length(data$stat)),col="black",lty=2) 
  }
else{}

minc=min(round(subdata$pos/ 1e6))
maxc=max(round(subdata$pos/ 1e6))
seqminmax=seq(minc,maxc,by=everymb)
axis=axis(side=1, at=seqminmax, labels=seqminmax)


}

# plot 2
}else if(type=="cumulative"){
print(paste0('to plot a manhattan in different rows per chromosome, use flag type perchromosome'))

transitions<-tapply(data$pos,data$chr,min)
whichtransitions<-row.names(data[ which( paste(data$chr,data$pos) %in% paste(names(transitions), as.numeric(transitions) ) )  , ])

if(!is.null(pdfname)){  write.pdf(file = pdfname,height = 173/4 ) }

data$sumcum<-1:dim(data)[1]
data$chr<-as.numeric(data$chr)

plot(y=(data$stat), x= (data$sumcum ) ,pch=16,cex=sizes,col=chrpalette[data$chr],xlab="Position (Mb)",  ylab= ylab  ,xaxt="n",frame.plot = F)
# plot(y=(data$stat), x= (data$sumcum ) ,pch=19,col=chrpalette[data$chr],xlab="Position (Mb)",  ylab= ylab  ,xaxt="n",frame.plot = F)
abline(v=whichtransitions)

if(!is.null(empirical) ){
  abline(h=-log10(0.05),col="black",lty=2)
  abline(h=-log10(0.01),col="black",lty=2)
  abline(h=-log10(0.001),col="black",lty=2)
  }else{  abline(h=-log10(0.05/length(data$stat)),col="black",lty=2)}


minc=min(round(data$sumcum))
maxc=max(round(data$sumcum))
seqminmax=round(seq(minc,maxc,length.out = 20),digits =0 )

axis=axis(side=1, at=seqminmax, labels=round(data[seqminmax,'pos' ]/ 1e6),las=3)

}

if(!is.null(pdfname)){  dev.off() }
par(mfrow=c( 1,1),mar = c(5.1 ,4.1, 4.1, 2.1)) # this sets default par

}