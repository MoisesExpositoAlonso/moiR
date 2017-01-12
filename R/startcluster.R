#' Start cluster for parallel
#' 
#' @return A cluster object
#' @examples
#' startcluster()
#' @export
 

startcluster<-function(){
require("parallel")
print("starting cluster")
  #==================================
# Calculate the number of cores
no_cores <- detectCores() - 1
if(no_cores > 4){ no_cores=10}
 # Initiate cluster
cl <- makeCluster(no_cores,type="FORK")
# Now we just call the parallel version of lapply, parLapply:
#============================
return(cl)
}

#' Stop cluster
#' 
#' @param cl cluster object passed by startcluster
#' @return stops cluster
#' @examples
#' stopcluster(cl)
#' @export
 

stopcluster<-function(cl=cl){
  stopCluster(cl)
}

