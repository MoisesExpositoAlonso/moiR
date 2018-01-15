#' Quick plot + contrast hypothesis test to compare two populations
#'
#' @param y a numeric vector of group 1
#' @param x a numeric vector of group 2
#' @param method string, either t.student (parametric) or wilcox.test (nonparametric)
#' @param title string with the name of the plot
#' @param wantpoints Logical FALSE/TRUE to determine whether points will be overplotted
#' @param wantviolin Logical FALSE/TRUE to determine whether you want a violine plot in the background
#' @param The colors of the two groups. Default c("darkorchid4","aquamarine4")
#'
#' @return Prints a plot with the given test p-value. If title is provided, it prints into a pdf.
#'
#'
#' @export
#'


lm_eq <- function(y, x,tex=TRUE){
    mylm <- lm(y ~ x)
    p= format(coefficients( summary(mylm )) [2,4],digits = 3,scientific = TRUE)
    r2 = round(summary(mylm)$r.squared, digits = 3)
    b = round(coefficients( summary(mylm )) [2,1],digits = 3)

    if(tex==FALSE){
      return(sprintf( "R2= %s, b= %s, p= %s",r2,b,p))
    }else{
      return(paste0("$R^2 = $",r2,", $\\beta = $",b , ", $ p = $",p ))
    }
}

#' @export

r2_eq <- function(y, x,tex=TRUE){
    # mylm <- lm(y ~ x)
    mylm <- cor.test(y , x)
    p= format(mylm$p.value,digits = 3,scientific = TRUE)
    r2=round(mylm$estimate, digits = 3)
    

    if(tex==FALSE){
      
      return(sprintf( "r= %s, p= %s",r2,p))
    }else{
    	return(paste0("$r = $",r2, ", $ p = $",p ))
    }
}

