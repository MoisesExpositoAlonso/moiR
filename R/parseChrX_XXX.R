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
parseChrX_XXX<-function(strings){
  spl<-unlist(strsplit(gwanew$namesite,split = "_",fixed=T))
  chr<-gsub( spl[seq(1,length(spl),by=2)], pattern = "Chr",replacement = "")
  pos<-spl[seq(2,length(spl),by=2)]
  toreport<-data.frame(Chromosome=as.numeric(chr),Position=as.numeric(pos) )
  return(toreport)
}
makeChrX_XXX<-function(chr,pos){
dataChrX_XXX<-paste0("Chr",chr,"_",pos)
return()
}