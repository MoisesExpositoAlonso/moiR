#' Make formula object from a response and multiple predictor variable names. Useful for glm framework.
#'
#' @param response variable response, only one string.
#' @param predictors predicting variables, vector of strings.
#' @return A formula object that can be feed to lm, glm, aov, randomForest, etc.
#' @export
#' @examples
#' makeformula(response="phenotype",predictors=c("bio1","bio2"))
#' @export

makeformula<-function(response,predictors, asformula=T){
t <- paste0(predictors ,collapse = " + ")
t1 <- paste(response," ~", t)
if(asformula==T){
  myformula <- as.formula(t1)
}else{
  myformula <- t1
}
return(myformula)
}
