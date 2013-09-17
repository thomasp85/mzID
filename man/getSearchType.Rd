\name{getSearchType}
\alias{getSearchType}
\title{Gets the search type as specified in the mzIdentML file}
\usage{
  getSearchType(doc, ns)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{The appropriate namespace for the doc, as a
  named character vector with the namespace named x}
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

