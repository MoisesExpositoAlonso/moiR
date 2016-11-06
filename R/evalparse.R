#' Wrap of functions eval and parse for less code writing
#' 
#' @param the function with the required arguments given as a single string

#'
#' @return runs the passed function.

#'
#' @details
#' This is like the function \code{\link[stats]{fisher.test}}, but
#'   calculates an approximate P-value rather than performing a complete
#'   enumeration.  This will be better for large, sparse tables.
#'
#' @importFrom stats chisq.test
#' @export
#' @return
#' Returns whatever the function with its given arguments will return.
#'
#' @examples
#' sumof5and4 <- evalparse(paste("sum(",4,"+",5,")") )
#'
#' @seealso
#' \code{\link[base]{eval}}, \code{\link[base]{parse}} 
#'
#' @keywords
#'

evalparse<-function(x){
  return(eval(parse(text=x)))
}

