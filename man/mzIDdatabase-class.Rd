% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzIDdatabase.R
\docType{class}
\name{mzIDdatabase-class}
\alias{database,mzIDdatabase-method}
\alias{length,mzIDdatabase-method}
\alias{mzIDdatabase-class}
\alias{show,mzIDdatabase-method}
\title{A class to store database information from an mzIdentML file}
\usage{
\S4method{show}{mzIDdatabase}(object)

\S4method{length}{mzIDdatabase}(x)

\S4method{database}{mzIDdatabase}(object, safeNames = TRUE)
}
\arguments{
\item{object}{An mzIDevidence object}

\item{x}{An mzIDdatabase object}

\item{safeNames}{Should column names be lowercased to ensure compatibility
between v1.0 and v1.1 files?}
}
\description{
This class handles parsing and storage of database information from mzIDentML
files, residing at the /MzIdentML/SequenceCollection/DBSequence node.
}
\details{
The content of the class is stored in a data.frame with columns depending on 
the content of the mzIdentML file. Required information for files conforming 
to the mzIdentML standard are: 'accession', 'searchDatabase_ref' and 'id', 
while additional information can fx be 'length' (number of residues), 
'description' (from the fasta file) and 'sequence' (the actual sequence).
}
\section{Methods (by generic)}{
\itemize{
\item \code{show}: Short summary of the content of the object

\item \code{length}: Report the number of proteins in the database

\item \code{database}: Get the database used for searching
}}
\section{Slots}{

\describe{
\item{\code{database}}{A data.frame containing references to all the database 
sequences from the mzIdentML file}
}}
\section{Objects from the class}{

Objects of mzIDdatabase are not meant to be created explicitly but as part of
the \code{\link{mzID-class}}. Still object can be created with the 
constructor \code{\link{mzIDdatabase}}.
}
\seealso{
\code{\link{mzIDdatabase}}

Other mzID.classes: \code{\link{mzID-class}},
  \code{\link{mzIDCollection-class}},
  \code{\link{mzIDevidence-class}},
  \code{\link{mzIDparameters-class}},
  \code{\link{mzIDpeptides-class}},
  \code{\link{mzIDpsm-class}}
}

