\name{getFragmentTolerance}
\alias{getFragmentTolerance}
\title{Looks for presence of fragment tolerance settings and reports these if found}
\usage{
getFragmentTolerance(doc, ns, addFinalizer = FALSE)
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
A \code{data.frame} with the settings used for fragment
tolerance or \code{NULL} if none exists
}
\description{
This function checks for the existence of
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/FragmentTolerance
and if found reports these.
}
\seealso{
\code{\link{mzIDparameters-class}}
}

