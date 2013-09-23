\docType{methods}
\name{flatten}
\alias{flatten}
\alias{flatten,mzID-method}
\alias{flatten,mzID,ANY-method}
\alias{flatten,mzIDpeptides-method}
\alias{flatten,mzIDpeptides,ANY-method}
\alias{flatten,mzIDpsm-method}
\alias{flatten,mzIDpsm,ANY-method}
\title{Flatten an mzID related class into a table}
\arguments{
  \item{object}{The object to be flattened}

  \item{...}{Additional parameters (currently unused)}
}
\value{
  A \code{data.frame} with the flattened result
}
\description{
  This function flattens the content of the object into a
  table by merging the content intelligently (it knows the
  links between the different objects).
}
\examples{
mzResults <- mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_tandem.mzid")
head(flatten(mzResults))
}
\seealso{
  \code{\link{mzID-class}}
}

