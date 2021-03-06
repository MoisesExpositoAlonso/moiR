% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cormatrix.R
\name{cormatrix_r_p}
\alias{cormatrix_r_p}
\title{Correlations of all pairs o variables.}
\usage{
cormatrix_r_p(numericdatanona, columnpos = NULL, filename = NULL,
  method = "pearson")
}
\arguments{
\item{numericdatanona}{A numeric data frame}

\item{columnpos}{In case you want to compute correlations of a subset of columns. Default NULL}

\item{filename}{Optional parameter, if provided a string, it will output the plot in a pdf in the working directory. Default NULL}

\item{method}{the methods inherited from rcorr. Default "pearson". Other possibility is "spearman".}
}
\value{
If a file name is indicated, an output pdf plot. Otherwise, prints to the graphic device.

A matrix with lower triangle R pearson correlations, and upper triangle the p-values
}
\description{
Correlations of all pairs o variables.
}
\examples{
library(MASS)
data(mtcars)
cormatrix_r_p(as.numeric(na.omit(mtcars)))

}

