\name{getModifications}
\alias{getModifications}
\title{Looks for presence of amino acid modification settings and reports these if found}
\usage{
getModifications(doc, ns, addFinalizer = FALSE)
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
A \code{data.frame} with information on the modification
settings in the search or \code{NULL} if none exists
}
\description{
This function checks for the existence of
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/ModificationParams
and if found formats these for easy reading.
}
\seealso{
\code{\link{mzIDparameters-class}}
}

