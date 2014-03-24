\name{getDatabaseFilter}
\alias{getDatabaseFilter}
\title{Looks for any database filters applied during the peptide search}
\usage{
getDatabaseFilter(doc, ns, addFinalizer = FALSE)
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
\code{NULL}
}
\description{
This function is still unimplemented, as no proper use of
the
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/DatabaseFilters
have been found
}
\seealso{
\code{\link{mzIDparameters-class}}
}

