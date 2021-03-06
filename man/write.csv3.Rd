% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.csv3.R
\name{write.csv3}
\alias{write.csv3}
\title{Customized write.csv function.}
\usage{
write.csv3(table, file)
}
\arguments{
\item{table}{data.frame, vector, matrix to write}

\item{file}{filename. Include .csv ending}
}
\value{
outputs a table
}
\description{
Customized write.csv function.
}
\examples{
\dontrun{
mymatrix<-matrix(ncol=2, c(1,2,3,4))
write.csv2(examplematrix,"myexample.csv")
}

}

