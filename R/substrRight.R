#' Extract characters of a string from the right side of it.
#' 
#' @param x string
#' @param lastpos from what position counting in the right you want to extract characters
#' @param giveright logical True/False whether you want to extract the strings in the left form that number of rom the right.
#' @return The strings in the left of in the right from a given position counting from the right of a word
#' @examples
#' substrRight(monkey, 2,giveright=T)
#' substrRight(monkey, 2,giveright=F)
#' @export

substrRight <- function(x, lastpos,giveright=T){
  if(giveright==T){
  substr(x, nchar(x)-lastpos+1, nchar(x))
  }else{
     substr(x, 1,nchar(x)-lastpos+1)
   }
}