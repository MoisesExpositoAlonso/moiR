#' Get the FDR threshold from a set of p-values
#'
#' @param pval
#'
#' @return
#' @details
#' The function will do a p-value adjustment using p.adjust(method='fdr') and
#' then will search for the p-value in the vector provided that corresponds to
#' that element that is below the alpha level. If there is none, it will report
#' the minimum p-value after the FDR transformation, which will be above the
#' alpha level and then indicates that none of the provided p-values would be
#' significant after the FDR correction.
#'
#' @export
#'
#' @examples
fdr_level<-function(pval,al=0.05){
  stopifnot(class(pval) == 'numeric')

  pad= p.adjust(pval,method = 'fdr')

  tmp=data.frame(pad=pad,pval=pval)
  tmp=dplyr::arrange(tmp,pad)
  head(tmp)
  tail(tmp)


  signis=pval[pad<al]

  whatmin=tmp$pad[tmp$pad == min(tmp$pad) ]

  res=ifelse(length(signis) !=0,
                max(signis),
                min(pad))    # if there is none that is significative, just the smallest value

  return(res)
}
