\name{getMassTable}
\alias{getMassTable}
\title{Search for the presence of a mass table used during the search}
\usage{
getMassTable(doc, ns, addFinalizer = FALSE)
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
A \code{data.frame} with the masses used for each amino
acid at the different ms levels \code{NULL} if none exists
}
\description{
This function checks for the existence of
/MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/MassTable
and if found formats these for easy reading.
}
\seealso{
\code{\link{mzIDparameters-class}}
}

