\name{getSoftware}
\alias{getSoftware}
\title{Retrive information on the software used in the analysis}
\usage{
getSoftware(doc, ns, addFinalizer = FALSE)
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
A \code{data.frame} with a row for each software used, and
columns with at least name and id of the software
}
\description{
This function collects the information regarding the
analysis software used during creation of the data stored
in the mzIDentML file
}
\seealso{
\code{\link{mzIDparameters-class}}
}

