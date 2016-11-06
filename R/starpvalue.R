#' Transform a vector of p-values into ***, **, *, ., ns
#' 
#' @param x A numeric vector of p-values
#' @return A character vector of stars that represent simbolically significance
#' @examples
#' starpvalue(x=c(0.0002354, 0.658))
#' @export
 

starpvalue<-function(x){

if( !is.numeric(x))
{print("need to provide a number!")}

allstars<-sapply(x,FUN=function(x){
if(x<0.001){
  stars<-"***"
} else if(x<0.01){
  stars<-"**"
}else if(x<0.05){
    stars<-"*"
}else if(x<0.1){
    stars<-"."
}else{stars<-"ns"}
return(stars)
}
)

return(allstars) 

}