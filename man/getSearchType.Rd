\name{getSearchType}
\alias{getSearchType}
\title{Gets the search type as specified in the mzIdentML file}
\usage{
getSearchType(doc, ns, addFinalizer = FALSE)
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
A \code{character} with the name of the search type (e.g.
'ms-ms search' or 'de novo search')
}
\description{
This function simply extracts the information stored in
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/SearchType/cvParam/@name
}
\seealso{
\code{\link{mzIDparameters-class}}
}

