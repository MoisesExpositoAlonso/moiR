#' Force numeric
#' For when you have to transform a data.frame to a vector of numbers (e.g. for plotting), to save two long commands
#'
#' @param data.frame data frame to transform. It must contain only values that can be transformed to numeric
#'
#' @return A numeric vector
#'
#' @examples
#'
#' @export
#'
fn<-function(data.frame){
	as.numeric(as.matrix(data.frame))
}
fc<-function(data.frame){
	as.character(as.matrix(data.frame))
}
