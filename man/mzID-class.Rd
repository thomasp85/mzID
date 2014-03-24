\docType{class}
\name{mzID-class}
\alias{mzID-class}
\title{A class to contain data from mzIdentML-files}
\description{
This class stores all parsed information from mzIdentML
files
}
\details{
The mzID class stores information in a subset of classes,
each class having its own slot. While These classes should
not need to be accessed directly, but descriptions of their
content is delegated to each respective class.
}
\section{Objects from the class}{
  Objects can be created using the \code{\link{mzID}}
  constructor, which handles parsing of mzIdentML files
}

\section{Slots}{
  \describe{ \item{\code{parameters}:}{An instance of
  \code{\link{mzIDparameters-class}}. This object contains
  all information related to how the analysis was carried
  out.} \item{\code{psm}:}{An instance of
  \code{\link{mzIDpsm-class}}. This object contains the
  meat of the analysis with all scans and their related
  PSMs recorded.} \item{\code{peptides}:}{An instance of
  \code{\link{mzIDpeptides-class}}. This object contains a
  library of all peptides generated from the database along
  with possible modifications.} \item{\code{evidence}:}{An
  instance of \code{\link{mzIDevidence-class}}. This object
  lists all peptides detected in the analysis with
  reference to the \code{mzIDpeptides} instance.}
  \item{\code{database}:}{An instance of
  \code{\link{mzIDdatabase-class}}. This object contains
  information on the proteins in the database. As the full
  database is not recorded in mzIdentML files the actual
  protein sequence is not recorded but there is sufficient
  information to retrieve it from the database file.} }
}

\section{Methods}{
  \describe{ \item{\code{\link{length}}:}{Reports the
  number of PSMs in the object.}
  \item{\code{\link{flatten}}:}{...} }
}
\references{
\url{http://www.psidev.info/mzidentml}
}
\seealso{
\code{\link{mzID}} \code{\link{mzIDparameters-class}}
\code{\link{mzIDpsm-class}}
\code{\link{mzIDpeptides-class}}
\code{\link{mzIDevidence-class}}
\code{\link{mzIDdatabase-class}}
}

