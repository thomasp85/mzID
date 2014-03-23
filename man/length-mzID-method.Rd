\docType{methods}
\name{length,mzID-method}
\alias{length,mzID-method}
\title{Report the length of an mzID object}
\usage{
\S4method{length}{mzID}(x)
}
\arguments{
  \item{x}{An mzID object}
}
\value{
A \code{numeric} giving the number of PSMs in the mzID
object
}
\description{
The length of an mzID object is defined as the number of
PSMs. An empty object has a length of 0
}
\seealso{
\code{\link{mzID-class}}
}

