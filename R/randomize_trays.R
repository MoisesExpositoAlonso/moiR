# #' Quick scatter plot with points colored by a 3rd variable
# #'
# #' @param accessions A vector of IDs of your genotypes
# #' @param replicates
# #' @param rowsize
# #' @param colsize
# #'
# #' @return A randomized list of genotypes on pots of a tray. You can specify if you also want a pseudo-geographic position of genotypes.
# #'
# #' @examples
# #'
# #'
#
#
# randomize_genotype_trays<-function(accessions, replicates, rowsize, colsize){
# set.seed(1)
# accessionsid<-c("999","333","666","992","332","662","991","331","661","990","330","660") ## this is an example
# numaccessions<-length(accessionsid)
#
# ## also provide the number of replicates and the size of your tray
# repnum<-8
# sizetray<-30
# colsize<-5
# rowsize<-6
# mytray<-matrix(ncol=colsize,nrow=rowsize)
# mytray<-matrix(c(1:40),ncol=colsize,nrow=rowsize,byrow=T)
# totsample=numaccessions*repnum
# experimentrandomized<-matrix(ncol=numaccessions,nrow=1)
# for (i in 1:repnum){
# repi<-sample(accessionsid,replace=F)
# experimentrandomized<- rbind(experimentrandomized,repi)
# # rbind(experimentrandomized,as.matrix(acc218[repi ,]) )
# }
# experimentrandomized<-experimentrandomized[-1,]
# ### allocate each individual replicate to a position in the tray
# dim(experimentrandomized)
# replicate<-sort(rep(1:repnum,times=numaccessions ) )
# length(replicate)
# ceiling(totsample/sizetray)->totaltrays
# trayid<-sort(rep(1:totaltrays,sizetray))
# length(trayid)
# trayid<-trayid[1:totsample]
# length(trayid)
# possiblepositions<-c(1:sizetray)
# positiontray<-rep(possiblepositions,times=totaltrays)
# length(positiontray)
# positiontray<-positiontray[1:totsample]
# length(positiontray)
# dim(experimentrandomized)
# finexperimentrandomized<-c(experimentrandomized)
# finexperimentrandomized<-cbind(finexperimentrandomized,trayid,positiontray,replicate)
# head(finexperimentrandomized)
# tail(finexperimentrandomized)
# ## provide a csv file name to write the data
#
# if(!is.null(filename)){
# filename<-"myfile.csv"
# write.tsv(finexperimentrandomized,	filename=paste(filename,"_",Sys.Date(),'.tsv'))
# }
#
# }
