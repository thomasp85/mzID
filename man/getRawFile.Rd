\name{getRawFile}
\alias{getRawFile}
\title{Retrive information on the raw files used in the analysis}
\usage{
  getRawFile(doc, ns)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{The appropriate namespace for the doc, as a
  named character vector with the namespace named x}
}
\value{
  A \code{data.frame} with a row for each raw data file
  used in the analysis and at least a column for the
  location and the ID format of the file.
}
\description{
  This function collects the information regarding the raw
  data files related to the analysis stored in the
  mzIdentML file
}
\seealso{
  \code{\link{mzIDparameters-class}}
}

