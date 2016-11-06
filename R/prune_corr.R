#' Prune a set of variables by a certain correlation threshold
#' 
#'
#' @param thedata A numeric matrix of variables
#' @param  threshold The threshold of correlation coefficient. Default 0.8 
#' @return A numeric matrix subset of thedata without the highly correlated variables
#' @examples
#' data(mtcars)
#' prune_corr(mtcars)  
cor_melt_sub<-function(thedata,threshold=.8){
  require(reshape2)
corr.matrix <- cor(thedata)
cormelt<-melt(corr.matrix)
cormeltsub<-subset(cormelt, cormelt$value>threshold & cormelt$Var1 !=cormelt$Var2)
cormeltsub$Var1 <- as.character.factor(cormeltsub$Var1)
cormeltsub$Var2 <- as.character.factor(cormeltsub$Var2)
return(cormeltsub)
}

#' Iterative prunning of a set of variables by a certain correlation threshold.
#' @param  iterations The algorithm function will stop when no more variables are correlated 
#'     over the specificed threshold or given a number of iterations. This is importante for big
#'     datasets with thousands of variables, which perhaps is not that appropriate for the function.
#' @inheritParams cor_melt_sub
prune_corr<-function(thedata, threshold=.8, iterations=10){
  cormeltsub<-cor_melt_sub(thedata,threshold)
  prundata<-thedata
  
for (i in 1:iterations)
  if( dim(cormeltsub)[1] != 0) {
  print(paste("iteration to prune variable num:", i))
    
    toremove<-cormeltsub[1,1]
  print(paste("removing variable ...", toremove))
    prundata<-prundata[,-which(names(prundata) ==toremove )]
    colnames(prundata)
    cormeltsub<-cor_melt_sub(thedata = prundata,threshold=threshold)
    
  }else{
  print("no more correlations over the threshold") 
  break 
  }
  return(prundata)
  
}
