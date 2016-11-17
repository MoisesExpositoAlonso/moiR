#' Quick scatter plot with points colored by a 3rd variable
#'
#' @param data A data.frame or matrix, with colnames 'chr', 'pos', and 'pval'.
#' @param chrpalette A vector of colors or single color
#' @param type Wether you want each chromosome separatedly plotted or all together. String "cumulative" or "perchromosome". Default cumulative
#' @param nameplot	data A dataset with 3 columns. It is assumed that 1 is x, 2 is y, 3 is varcol. This substitutes the use of x, y and varcol.
#' @param pdfname	pdf name to export plot
#' @param res	Numeric with the resoution to which chromosome positions wants to be round. Default Megabase.
#'
#' @return A scatter plot from base plot with points colored by the rank of a given variable
#'
#' @export
#'
manhattan<-function(data,
	chrpalette=c('black','black','black','black',"black"),
	empirical=NULL,
	type='cumulative',
	nameplot='',pdfname=paste0(nameplot, 'manhattan',".pdf"),
	res=1e6
	)
{

numchr=length(unique(data$chr))

print(paste0('printing manhattan plot to ...', pdfname))

if(type =='perchromosome'){
print(paste0('you can also output the manhattan in a single plot with the flag= type="cumulative" '))

write.pdf(file = pdfname,height = 40*numchr )
par(mfrow=c( numchr,1),mar = c(4,4,1.5,1))

for (chromnum in sort(unique(data$chr) )){

subdata<-subset(data,data$chr==chromnum)
subdata$chr<-as.numeric(subdata$chr)
maxchrompos<-max(data$pos)
#chcol<-c('firebrick3','lightskyblue','palegreen3','orchid4',"tan2")


sizes=-log10(subdata$pval)/max(-log10(subdata$pval))
plot(y=-log10(subdata$pval), x= (subdata$pos / 1e6) ,pch=16,cex=sizes,col=chrpalette[subdata$chr],xlab="Position (Mb)",  ylab= "log10 p-values"  ,xaxt="n",xlim=c(0,maxchrompos/1e6),frame.plot = F,everymb=1)

if(!is.null(empirical) ){
  abline(h=-log10(0.05),col="black",lty=2)
  abline(h=-log10(0.01),col="black",lty=2)
  abline(h=-log10(0.001),col="black",lty=2)
  }else{  abline(h=-log10(0.05/length(data$pval)),col="black",lty=2) }

minc=min(round(subdata$pos/ 1e6))
maxc=max(round(subdata$pos/ 1e6))
seqminmax=seq(minc,maxc,by=everymb)
axis=axis(side=1, at=seqminmax, labels=seqminmax)
}

dev.off()

}else if(type=="cumulative"){
print(paste0('you can also output the manhattan in a different rows pwer chromosome with the flag type="perchromosome" '))

numchrom=length(unique(data$chr))

transitions<-tapply(data$pos,data$chr,min)
whichtransitions<-row.names(data[ which( paste(data$chr,data$pos) %in% paste(names(transitions), as.numeric(transitions) ) )  , ])

pdf(file = pdfname,width=12,height = 5 ,useDingbats = F)

data$sumcum<-1:dim(data)[1]
data$chr<-as.numeric(data$chr)

sizes=-log10(data$pval)/max(-log10(data$pval))
plot(y=-log10(data$pval), x= (data$sumcum ) ,pch=16,cex=sizes,col=chrpalette[data$chr],xlab="Position (Mb)",  ylab= "log10 p-values"  ,xaxt="n",frame.plot = F)
abline(v=whichtransitions)

if(!is.null(empirical) ){
  abline(h=-log10(0.05),col="black",lty=2)
  abline(h=-log10(0.01),col="black",lty=2)
  abline(h=-log10(0.001),col="black",lty=2)
  }else{  abline(h=-log10(0.05/length(data$pval)),col="black",lty=2)}


minc=min(round(data$sumcum))
maxc=max(round(data$sumcum))
seqminmax=round(seq(minc,maxc,length.out = 20),digits =0 )

axis=axis(side=1, at=seqminmax, labels=round(data[seqminmax,'pos' ]/ 1e6),las=3)

dev.off()
}
}