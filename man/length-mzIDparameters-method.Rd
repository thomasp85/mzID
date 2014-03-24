\docType{methods}
\name{length,mzIDparameters-method}
\alias{length,mzIDparameters-method}
\title{Report the length of an mzIDparameters object}
\usage{
\S4method{length}{mzIDparameters}(x)
}
\arguments{
  \item{x}{An mzIDparameters object}
}
\value{
A \code{numeric} giving the number raw datafiles used in
the analysis
}
\description{
The length of an mzIDparameters object is defined as the
number of raw datafiles used in the analysis. An empty
object has a length of 0
}
\seealso{
\code{\link{mzIDparameters-class}}
}

