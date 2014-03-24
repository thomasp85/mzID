\docType{methods}
\name{length,mzIDevidence-method}
\alias{length,mzIDevidence-method}
\title{Report the length of an mzIDevidence object}
\usage{
\S4method{length}{mzIDevidence}(x)
}
\arguments{
  \item{x}{An mzIDevidence object}
}
\value{
A \code{numeric} giving the number of peptide evidences in
the mzIDevidence object
}
\description{
The length of an mzIDevidence object is defined as the
number of peptide evidences. An empty object has a length
of 0
}
\seealso{
\code{\link{mzIDevidence-class}}
}

