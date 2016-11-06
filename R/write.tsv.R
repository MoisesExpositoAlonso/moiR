#' Customized write.table function with table separated values.
#' 
#' @param table data.frame, vector, matrix to write
#' @param file filename. Include .tsv ending
#' @return outputs a table
#' @examples
#' \dontrun{
#' examplematrix<-matrix(ncol=2, c(1,2,3,4))
#' write.tsv(examplematrix,"myexample.tsv")
#' }


write.tsv<-function(table,file){
  write.table(table,file,row.names=F,quote=F,sep="\t")
}