#' Transform factor to numeric
#' I do not know why this is not in base package.
#' @parameter a factor object that you want to convert to numeric format 
#' @return A numeric vector
#' @examples
#' as.numeric.factor(as.factor(c(1:5)))
#' @export
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
