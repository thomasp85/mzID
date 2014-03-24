\name{getEnzymes}
\alias{getEnzymes}
\title{Extracts the enzymes used for digestion specified as specified in the software analysis}
\usage{
getEnzymes(doc, ns, addFinalizer = FALSE)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{The appropriate namespace for the doc, as a
  named character vector with the namespace named x}

  \item{addFinalizer}{\code{Logical} Sets whether reference
  counting should be turned on}
}
\value{
A \code{data.frame} with names of the enzymes as well as
other settings related to the cleavage of proteins or
\code{NULL} if no information is present
}
\description{
The function looks for information on the enzymes used in
the protocol and if present extracts it
}
\seealso{
\code{\link{mzIDparameters-class}}
}

