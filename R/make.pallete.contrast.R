#' Add contrast to a color palette
#' 
#' @param vecol A vector of colors
#' @param contrast Interger with the number of iterations of contrast desired
#' @param raw Logical. Do you want the raw colors or the pallete function?
#'
#' @return A colorRampPallete object with the desired contast if any
#'
#' @examples
#' make.pallete.contrast(c("red","orange","yellow","green","blue"), contrast=2)
#'
#' @export
make.pallete.contrast<-function(vecol,contrast=1,raw=F){
  if(contrast==0)
  {  
  	mypalette<-(vecol) 
  }
  else
  { 
  	for(i in 1:contrast){
    vecol<-c(vecol[1],vecol,vecol[length(vecol)])
    mypalette<-(vecol)
    }
  }


if(raw==T){
  return(mypalette )
}else{
  return(colorRampPalette(mypalette) )
}

}