\name{getDatabaseTranslation}
\alias{getDatabaseTranslation}
\title{Looks whether a translation table is present in the mzIdentML file and reports it if found}
\usage{
getDatabaseTranslation(doc, ns, addFinalizer = FALSE)
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
A \code{data.frame} with a translation table or \code{NULL}
if none exists
}
\description{
This function checks for the existence of
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/DatabaseTranslation
and if found formats these for easy reading.
}
\seealso{
\code{\link{mzIDparameters-class}}
}

