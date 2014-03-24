\docType{class}
\name{mzIDpsm-class}
\alias{mzIDpsm-class}
\title{A class to store psm information from an mzIdentML file}
\description{
This class handles parsing and storage of scan info and the
related psm's. This information resides in the
/*/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult
node.
}
\details{
The content of the class is stored as two data frames: One
containing a row for each scan in the results, and one
containing all psm's in the results. Additionally a list
containing indexing from scan to psm is stored.
}
\section{Objects from the class}{
  Objects of mzIDpsm are not meant to be created explicitly
  but as part of the \code{\link{mzID-class}}. Still object
  can be created with the constructor \code{\link{mzIDpsm}}
  (not exported).
}

\section{Slots}{
  \describe{ \item{\code{scans}:}{A data.frame containing
  all reference to all scans with at least one psm. The
  columns gives at least an ID, a spectrumID and a
  reference to the file used} \item{\code{id}:}{A
  data.frame containing all psm's from the analysis. The
  columns depend on the file but at least id, chargeState,
  experimentalMassToCharge, passThreshold and rank must
  exist according to the mzIdentML specifications}
  \item{\code{mapping}:}{A list with an entry for each row
  in @scans. Each entry contains an integer vector pointing
  to the related rows in @id} }
}

\section{Methods}{
  \describe{ \item{\code{length}:}{Reports the number of
  psm's} }
}
\seealso{
\code{\link{mzID-class}} \code{\link{mzIDpsm}}
}

