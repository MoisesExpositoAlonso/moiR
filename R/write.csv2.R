#' Customized write.csv function.
#' 
#' @param table data.frame, vector, matrix to write
#' @param file filename. Include .csv ending
#' @return outputs a table
#' @examples
#' \dontrun{
#' examplematrix<-matrix(ncol=2, c(1,2,3,4))
#' write.csv2(examplematrix,"myexample.csv")
#' }
#' 
#' @export
write.csv2<-function(table,file){
  write.csv(table,file,row.names=F,quote=F)
}