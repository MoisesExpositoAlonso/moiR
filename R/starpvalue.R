#' Transform a vector of p-values into ***, **, *, ., ns
#'
#' @param x A numeric vector of p-values
#' @return A character vector of stars that represent simbolically significance
#' @examples
#' starpvalue(x=c(0.0002354, 0.658))
#' @export


starpvalue<-function(x,samelength=F){

if( !is.numeric(x)){print("need to provide a number!")}

allstars<-sapply(x,FUN=function(x){
if(x<0.001){
  # stars<-"***"
    stars<-ifelse(samelength==F,"***","***")
}else if(x<0.01){
  # stars<-"**"
  stars<-ifelse(samelength==F,"**","**-")
}else if(x<0.05){
    # stars<-"*"
    stars<-ifelse(samelength==F,"*","*--")
}else if(x<0.1){
    # stars<-"."
    stars<-ifelse(samelength==F,".",".--")
}else{
    # stars<-"ns"
    stars<-ifelse(samelength==F,"ns","ns-")
}
return(stars)
}

)

return(allstars)

}
