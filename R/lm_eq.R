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


lm_eq <- function(y, x){
    mylm <- lm(y ~ x)
    p= format(coefficients( summary(mylm )) [2,4],digits = 3)
    r2 = format(summary(mylm)$r.squared, digits = 3)

    sprintf( "r2= %s, p= %s",r2,p)
}

