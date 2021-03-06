% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transparent.R
\name{transparent}
\alias{transparent}
\title{Make a color transparent
This function applies the level of transparency desired for a color or vector of colors}
\usage{
transparent(col, alpha = 0.5)
}
\arguments{
\item{alpha}{Numerical between 0 and 1.}

\item{color}{HEX code or name.}
}
\value{
The color HEX after applying a number of transparency
}
\description{
Make a color transparent
This function applies the level of transparency desired for a color or vector of colors
}
\examples{
blacktransparent<-transparent("black",alpha=0.5)

}

