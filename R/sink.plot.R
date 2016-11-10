sink.plot<-function(activebinding,name=deparse(substitute(activebinding))){
	write.pdf(paste0(name,".pdf"))
	activebinding
	dev.off()
}