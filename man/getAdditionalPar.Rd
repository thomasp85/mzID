\name{getAdditionalPar}
\alias{getAdditionalPar}
\title{Extracts the additional, often software specific parameters}
\usage{
getAdditionalPar(doc, ns, addFinalizer = FALSE)
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
A \code{list} with names corresponding to the name
attribute of the node, and content corresponding to the
value attribute. If the node haven't got a a value
attribute the content is set to TRUE
}
\description{
This function extracts and formats the information stored
in the cvParam and userParam nodes in
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/AdditionalSearchParams
}
\seealso{
\code{\link{mzIDparameters-class}}
}

