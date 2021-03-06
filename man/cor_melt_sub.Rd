% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prune_corr.R
\name{cor_melt_sub}
\alias{cor_melt_sub}
\title{Prune a set of variables by a certain correlation threshold}
\usage{
cor_melt_sub(thedata, threshold = 0.8)
}
\arguments{
\item{thedata}{A numeric matrix of variables}

\item{threshold}{The threshold of correlation coefficient. Default 0.8}
}
\value{
A numeric matrix subset of thedata without the highly correlated variables
}
\description{
Prune a set of variables by a certain correlation threshold
}
\examples{
data(mtcars)
prune_corr(mtcars)

}

