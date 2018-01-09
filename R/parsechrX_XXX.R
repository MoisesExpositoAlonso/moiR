#' Function to transform from SNP name format ChrX_XXX to X and XXX; and vice versa
#' 
#' @param strings A vector of strings of ChrX_XXX format name.
#' @param chr A vector of chromosome numbers.
#' @param pos A vector of positions in chromosomes.
#' @return 
#' Data frame with the chromosome and snp position corresponding to the names.
#' Vector of strings with the SNP names in format ChrX_XXX
#' 
#' @examples
#' parseChrX_XXX(strings=c("Chr4_54135","Chr1_18965"))
#' makeChrX_XXX(chr=c("4", "1"),pos=c("54135","18965") )
#' @export



parsechrX_XXX<-function(strings,ignorechr=FALSE){
  spl<-unlist(strsplit(strings,split = "_",fixed=T))
  if(ignorechr==FALSE) { chr<-gsub( spl[seq(1,length(spl),by=2)], pattern = "chr",replacement = "")}
  pos<-spl[seq(2,length(spl),by=2)]
  toreport<-data.frame(Chromosome=as.numeric(chr),Position=as.numeric(pos) )
  return(toreport)
}

#' Function to transform from chromosome X and position XXX to my standart format ChrX_XXX
#' 
#' @param strings A vector of strings of ChrX_XXX format name.
#' @param chr A vector of chromosome numbers.
#' @param pos A vector of positions in chromosomes.
#' @return 
#' Data frame with the chromosome and snp position corresponding to the names.
#' Vector of strings with the SNP names in format ChrX_XXX
#' 
#' @examples
#' parseChrX_XXX(strings=c("Chr4_54135","Chr1_18965"))
#' makeChrX_XXX(chr=c("4", "1"),pos=c("54135","18965") )
#' @export

makechrX_XXX<-function(chr,pos){
dataChrX_XXX<-paste0("chr",chr,"_",pos)
return()
}
