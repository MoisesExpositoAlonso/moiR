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
  x= format(x,digits=digits)

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
}
