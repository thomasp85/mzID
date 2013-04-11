\name{mzID}
\alias{mzID}
\title{Parse an mzIdentML file}
\usage{
  mzID(file)
}
\arguments{
  \item{file}{A character string giving the location of the
  mzIdentML file to be parsed}
}
\value{
  An mzID object
}
\description{
  This function takes a single mzIdentML file and parses it
  into an mzID object.
}
\details{
  The mzID function uses the XML package to read the
  content of an mzIdentML file and store it in an mzID
  object. Unlike how mzR handles mzML files, mzID parses
  everything in one chunk. Memory can thus be a problem for
  very big datasets, but as mzIdentML files are not
  indexed, it is ineficient to access the data dynamically.
}

