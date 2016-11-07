#' Add contrast to a color palette
#' 
#' @vecol A vector of colors
#' @contrast Interger with the number of iterations of contrast desired
#' @return A colorRampPallete object with the desired contast if any
#' @examples
#' make.pallete.contrast(c("red","orange","yellow","green","blue"), contrast=2)
#' @export
make.pallete.contrast<-function(vecol,contrast=1){
  if(contrast==0)
  {  
  	mypalette<-colorRampPalette(vecol) 
  }
  else
  { 
  	for(i in 1:contrast){
    vecol<-c(vecol[1],vecol,vecol[length(vecol)])
    mypalette<-colorRampPalette(vecol)
    }
  }
return(mypalette)
}