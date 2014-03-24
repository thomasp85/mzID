\docType{class}
\name{mzIDdatabase-class}
\alias{mzIDdatabase-class}
\alias{mzIDparameters-class}
\title{A class to store database information from an mzIdentML file}
\description{
This class handles parsing and storage of database
information from mzIDentML files, residing at the
/MzIdentML/SequenceCollection/DBSequence node.

This class tries to collect the multitude of different
analysis information required to rerun the analysis. The
intended data to be stored are: The software used in the
analysis of the data, the location and nature of the
rawfile(s), the location and nature of the database
file(s), the location of the mzIDentML file itself as well
as all the parameters used during the analysis leading to
the mzIdentML file. Information regarding how the LC-MS
experiment was performed should be collected from the raw
data file. As the parameters used in different software
solutions can vary greatly, all these parameters are stored
in a named list, which can thus be very different from
pipeline to pipeline. It is the users responsibility to
check conformity between samples.
}
\details{
The content of the class is stored in a data.frame with
columns depending on the content of the mzIdentML file.
Required information for files conforming to the mzIdentML
standard are: 'accession', 'searchDatabase_ref' and 'id',
while additional information can fx be 'length' (number of
residues), 'description' (from the fasta file) and
'sequence' (the actual sequence).
}
\section{Objects from the class}{
  Objects of mzIDdatabase are not meant to be created
  explicitly but as part of the \code{\link{mzID-class}}.
  Still object can be created with the constructor
  \code{\link{mzIDdatabase}} (not exported).
}

\section{Slots}{
  \describe{ \item{\code{database}:}{A data.frame
  containing references to all the database sequences from
  the mzIdentML file} }
}

\section{Methods}{
  \describe{ \item{\code{length}:}{Reports the number of
  entries in the database} }
}

\section{Objects from the class}{
  Objects of mzIDparameters are not meant to be created
  explicitly but as part of the \code{\link{mzID-class}}.
  Still object can be created with the constructor
  \code{\link{mzIDparameters}} (not exported).
}

\section{Slots}{
  \describe{ \item{\code{software}:}{A data frame with
  information retaining to the software used for the
  analysis. At least the name and an id is given, but
  optionally also version number and URI}
  \item{\code{rawFile}:}{A data frame with information
  about the raw data file(s) used for the analysis. The
  data frame will contain at least the location and
  spectrum ID format.} \item{\code{databaseFile}:}{ A data
  frame containing at least the location and file format of
  the database file used in the search.}
  \item{\code{idFile}:}{A character string containing the
  location of the mzIdentML file at the time of parsing.}
  \item{\code{parameters}:}{A list containing containing
  the information stored in the
  MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol
  node. SearchType and Threshold are the only required
  parameters given by the mzIdentML standard.} }
}

\section{Methods}{
  \describe{ \item{\code{length}:}{Reports the number of
  raw files used in the analysis.} }
}
\seealso{
\code{\link{mzID-class}} \code{\link{mzIDdatabase}}

\code{\link{mzID-class}} \code{\link{mzIDparameters}}
}

