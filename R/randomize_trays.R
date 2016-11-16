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
#' plotwithcolors(x=x,y=y,colorvar=z)
#' 
#' @seealso ggplot_world_map
#' @export
#' 


randomize_genotype_trays<-function(){
##### R code to randomize your experiment
## you need to provide the IDs of your genotype, for instance in this case as a vector 
accessionsid<-c("a vector of IDs of your genotypes")
accessionsid<-c("999","333","666","992","332","662","991","331","661","990","330","660") ## this is an example
numaccessions<-length(accessionsid)

## also provide the number of replicates and the size of your tray
repnum<-8
sizetray<-30
colsize<-5
rowsize<-6
mytray<-matrix(ncol=colsize,nrow=rowsize)
mytray<-matrix(c(1:40),ncol=colsize,nrow=rowsize,byrow=T)
totsample=numaccessions*repnum
experimentrandomized<-matrix(ncol=numaccessions,nrow=1)
for (i in 1:repnum){
repi<-sample(accessionsid,replace=F)
experimentrandomized<- rbind(experimentrandomized,repi)
# rbind(experimentrandomized,as.matrix(acc218[repi ,]) )
}
experimentrandomized<-experimentrandomized[-1,]
### allocate each individual replicate to a position in the tray
dim(experimentrandomized)
replicate<-sort(rep(1:repnum,times=numaccessions ) )
length(replicate)
ceiling(totsample/sizetray)->totaltrays
trayid<-sort(rep(1:totaltrays,sizetray))
length(trayid)
trayid<-trayid[1:totsample]
length(trayid)
possiblepositions<-c(1:sizetray)
positiontray<-rep(possiblepositions,times=totaltrays)
length(positiontray)
positiontray<-positiontray[1:totsample]
length(positiontray)
dim(experimentrandomized)
finexperimentrandomized<-c(experimentrandomized)
finexperimentrandomized<-cbind(finexperimentrandomized,trayid,positiontray,replicate)
head(finexperimentrandomized)
tail(finexperimentrandomized)
## provide a csv file name to write the data

if(!is.null(filename)){
filename<-"myfile.csv"
write.tsv(finexperimentrandomized,	filename=paste(filename,"_",Sys.Date(),'.tsv'))
}

}