\docType{methods}
\name{flatten}
\alias{flatten}
\alias{flatten,mzID-method}
\alias{flatten,mzIDCollection-method}
\alias{flatten,mzIDpeptides-method}
\alias{flatten,mzIDpsm-method}
\title{Flatten an mzID related class into a table}
\usage{
flatten(object, ...)

\S4method{flatten}{mzID}(object, no.redundancy = FALSE)

\S4method{flatten}{mzIDCollection}(object, no.redundancy = FALSE)

\S4method{flatten}{mzIDpeptides}(object)

\S4method{flatten}{mzIDpsm}(object)
}
\arguments{
  \item{object}{The object to be flattened}

  \item{...}{Additional parameters (currently unused)}

  \item{no.redundancy}{\code{Logical} Should duplicate
  peptides be removed. Default is \code{FALSE} as identical
  peptides from different proteins should normally be
  kept.}
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

