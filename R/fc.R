#' Force character
#' @param data.frame data frame to transform. It must contain only values that can be transformed to character
#' @return A character vector
#' @export

fc<-function(data.frame){
  as.character(as.matrix(data.frame))
}
