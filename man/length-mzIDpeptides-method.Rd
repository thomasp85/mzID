\docType{methods}
\name{length,mzIDpeptides-method}
\alias{length,mzIDpeptides-method}
\title{Report the length of an mzIDpeptides object}
\usage{
\S4method{length}{mzIDpeptides}(x)
}
\arguments{
  \item{x}{An mzIDpeptides object}
}
\value{
A \code{numeric} giving the number of peptides in the
mzIDpeptides object
}
\description{
The length of an mzIDpeptides object is defined as the
number of peptides in the @peptides slot. An empty object
has a length of 0
}
\seealso{
\code{\link{mzIDpeptides-class}}
}

