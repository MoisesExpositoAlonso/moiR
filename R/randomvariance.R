#' Get the variance of random effects model
#'
#' @param lmod
#' @param var
#'
#' @return
#' @export
#'
#' @examples
randomvariance<-function(lmod,var=NULL, verbose=FALSE){

    if(class(lmod)=='lmerMod'){
        if(is.null(var)){
          if(verbose!=FALSE) message("No var provided, attempting to deduce")
          tmp=strsplit(as.character(lmod@call),split = c(','),fixed=TRUE)[[2]]
          tmp2=strsplit(tmp,split = "+",fixed=TRUE)[[1]]
          tmp3=tmp2[grep(x=tmp2,pattern = "(1 |", fixed = TRUE)]
          tmp4=gsub(x=tmp3,pattern = " (1 | ",replacement = "",fixed = TRUE)
          tmp5=gsub(x=tmp4,pattern = ")",replacement = "",fixed = TRUE)
          tmp6=gsub(x=tmp5,pattern = " ",replacement = "",fixed = TRUE)
          var=tmp6
        }
      # print(var)
      varcovar=as.data.frame(VarCorr(lmod))
      res<-dplyr::filter(varcovar, grp %in% var) [,"vcov"] / sum(varcovar[,"vcov"])
      res=as.matrix(res)

    }else if(class(lmod)=='MCMCglmm'){
        if(is.null(var)){
          if(verbose!=FALSE) message("No var provided, attempting to deduce")
          tmp= strsplit(as.character(lmod$Random$formula), split="+", fixed=TRUE)[[2]]
          var=gsub(x=tmp,pattern = " ",replacement = "",fixed = TRUE)
          }
      # print(var)
      res=h2MCMC(lmod,randomname=var)

    }else{
      stop("Don't know this model class")
    }

    return(res)
}


#' Proportion of variance of a random factor from a MCMCglmm output object
#'
#' @param mcmcglmmobject
#' @param randomname
#'
#' @return
#' @export
#'
#' @examples
h2MCMC=function(mcmcglmmobject, randomname){

  allvariance=apply(mcmcglmmobject$VCV, 1, sum)
  posterior<-mcmcglmmobject$VCV[,randomname]/ allvariance

  return(list(Mode=posterior.mode(posterior),HPD=HPDinterval(posterior,0.95)))

}

