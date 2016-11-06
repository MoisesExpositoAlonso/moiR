#' Tell me the time
#' This function returns the time
#' 
#' @return A time string
#' @examples
#' thetimenow<-telltime()
#' 
#' @export

telltime<-function(){
ptm <- proc.time()
return(ptm[1])
}

