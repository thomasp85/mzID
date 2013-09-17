\name{getDatabaseFile}
\alias{getDatabaseFile}
\title{Retrive information on the database used in the analysis}
\usage{
  getDatabaseFile(doc, ns)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{The appropriate namespace for the doc, as a
  named character vector with the namespace named x}
}
\value{
  A \code{data.frame} with a at least the location of the
  database stored
}
\description{
  This function collects the information regarding the
  database used in the analysis stored in the mzIdentML
  file
}
\seealso{
  \code{\link{mzIDparameters-class}}
}

