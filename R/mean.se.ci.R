#' Return a nicely formatted mean(ci) string from a numeric vector
#'
#' @param x
#' @param digits
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
mean.ci<-function(x,digits=3,sep=','){

  # round
  x= specify_decimal(x,digits)

  # collapse
  if(length(x) ==2){
    toreport= as.character(paste0(x[1]," (",x[2],")"))
  }else if(length(x)==3){
    toreport= as.character(paste0(x[1]," (",x[2], sep, " ",x[3],")"))
  }else{
    stop('Don not know this length')
  }

  # make the exponent x 10
  toreport=gsub(toreport,    pattern = "e",replacement=' x 10',fixed=T)
return(toreport)
}

mean.ci.v<-function(x,...){

  if(!any(dim(x) == 3)){
    stop("Need to provide a vector of length 3 or an array one of which dymensions is 3")
  }
# finish vectorized implementation
  mean.ci(x,...)

}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


mean_se<-function(x){
a <- mean(x,na.rm = TRUE)
s <- sd(x,na.rm = TRUE)
n <- length(x)
error <- qt(0.975,df=n-1)*s/sqrt(n)
l <- a-error
r <- a+error

mean.ci(c(a,l,r),digits = 2)

}
