\name{length}
\alias{length}
\title{Report the length of an mzIDpsm object}
\arguments{
  \item{x}{An mzIDpsm object}

  \item{x}{An mzIDpeptides object}

  \item{x}{An mzIDparameters object}

  \item{x}{An mzIDevidence object}

  \item{x}{An mzIDdatabase object}

  \item{x}{An mzID object}
}
\value{
  A \code{numeric} giving the number of psm's in the
  mzIDpsm object

  A \code{numeric} giving the number of peptides in the
  mzIDpeptides object

  A \code{numeric} giving the number raw datafiles used in
  the analysis

  A \code{numeric} giving the number of peptide evidences
  in the mzIDevidence object

  A \code{numeric} giving the number of entries in the
  mzIDdatabase object

  A \code{numeric} giving the number of PSMs in the mzID
  object
}
\description{
  The length of an mzIDpsm object is defined as the number
  of psm's. An empty object has a length of 0

  The length of an mzIDpeptides object is defined as the
  number of peptides in the @peptides slot. An empty object
  has a length of 0

  The length of an mzIDparameters object is defined as the
  number of raw datafiles used in the analysis. An empty
  object has a length of 0

  The length of an mzIDevidence object is defined as the
  number of peptide evidences. An empty object has a length
  of 0

  The length of an mzIDdatabase object is defined as the
  number of entries in the databse. An empty object has a
  length of 0

  The length of an mzID object is defined as the number of
  PSMs. An empty object has a length of 0
}
\seealso{
  \code{\link{mzIDpsm-class}}

  \code{\link{mzIDpeptides-class}}

  \code{\link{mzIDparameters-class}}

  \code{\link{mzIDevidence-class}}

  \code{\link{mzIDdatabase-class}}

  \code{\link{mzID-class}}
}

