\name{getThreshold}
\alias{getThreshold}
\title{Finds the psm threshold used in the file}
\usage{
  getThreshold(doc, ns)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{The appropriate namespace for the doc, as a
  named character vector with the namespace named x}
}
\value{
  A \code{data.frame} with columns 'name' and 'value'
  storing the tresholds used
}
\description{
  This function simply extracts the attributes stored in
  /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/Threshold/*
}
\seealso{
  \code{\link{mzIDparameters-class}}
}

