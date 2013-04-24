\name{mzIDpeptides}
\alias{mzIDpeptides}
\title{A constructor for the mzIDpeptides class}
\usage{
  mzIDpeptides(doc, ns)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{The appropriate namespace for the doc, as a
  named character vector with the namespace named x}
}
\value{
  An \code{mzIDpeptides} object
}
\description{
  This function handles parsing of data and construction of
  an mzIDpeptides object. This function is not intended to
  be called explicitly but as part of an mzID construction.
  Thus, the function is not exported.
}
\seealso{
  \code{\link{mzIDpeptides-class}}
}

