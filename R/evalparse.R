#' Wrap of functions eval and parse for less code writing
#' 
#' @param the function with the required arguments given as a single string
#'
#' @return runs the passed function.
#'
#' @details
#' @importFrom base eval
#' @export
#' @return
#' Returns whatever the function with its given arguments will return.
#'
#' @examples
#' sumof5and4 <- evalparse(paste("sum(",4,"+",5,")") )
#'
#' @seealso
#' \code{\link[base]{eval}}, \code{\link[base]{parse}} 
evalparse<-function(x){
  return(eval(parse(text=x)))
}

