#' Make formula object from a response and multiple predictor variable names. Useful for glm framework.
#' 
#' @param response variable response, only one string.
#' @param predictors predicting variables, vector of strings.
#' @return A formula object that can be feed to lm, glm, aov, randomForest, etc.
#' @examples
#' makeformula(response="phenotype",predictors=c("bio1","bio2"))
makeformula<-function(response,predictors){
t <- paste0(predictors ,collapse = " + ")
t1 <- paste(response," ~", t)
myformula <- as.formula(t1)
return(myformula)
}