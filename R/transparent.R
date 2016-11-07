#' Make a color transparent
#' This function applies the level of transparency desired for a color or vector of colors
#' @parameter color HEX code or name.
#' @parameter alpha Numerical between 0 and 1. 
#' @return The color HEX after applying a number of transparency
#' @examples
#' blacktransparent<-transparent("black",alpha=0.5)
#' 
#' @export
transparent<-function (col, alpha = 0.5)
{
    res <- apply(col2rgb(col), 2, function(c) rgb(c[1]/255, c[2]/255,
        c[3]/255, alpha))
    return(res)
}