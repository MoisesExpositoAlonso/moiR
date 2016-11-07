#' Transform the object name into string
#' Get the name of the object. This is useful when you want to use the name of the data that is used in a function, to name the output.
#' 
#' @return A string
#' @examples
#' thedataname<-getobjectname(mydata)
#' @export
#' 
get.object.name<-function(x){
  deparse(substitute(x))
}