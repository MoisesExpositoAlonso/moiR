#' Difference in time 
#' Given a time point in the past, this function gives you the difference
#' 
#' @return A time string
#' @examples
#' difference_in_time<-tooktime(thetimenow)
#' 
#' @export
tooktime<-function(timestart){
  cat(paste('time in sec', c(proc.time()[1] - timestart),
        '\ntime in min', c(proc.time()[1] - timestart)/60,'\n'  ))
}

