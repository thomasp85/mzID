% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzIDparameters.R
\docType{class}
\name{mzIDparameters-class}
\alias{files,mzIDparameters-method}
\alias{length,mzIDparameters-method}
\alias{mzIDparameters-class}
\alias{parameters,mzIDparameters-method}
\alias{show,mzIDparameters-method}
\alias{software,mzIDparameters-method}
\title{A Class to store analysis information from the mzIdentML file}
\usage{
\S4method{show}{mzIDparameters}(object)

\S4method{length}{mzIDparameters}(x)

\S4method{parameters}{mzIDparameters}(object)

\S4method{software}{mzIDparameters}(object)

\S4method{files}{mzIDparameters}(object)
}
\arguments{
\item{object}{An mzIDparameters object}

\item{x}{An mzIDparameters object}
}
\description{
This class tries to collect the multitude of different analysis information 
required to rerun the analysis. The intended data to be stored are: The 
software used in the analysis of the data, the location and nature of the 
rawfile(s), the location and nature of the database file(s), the location of 
the mzIDentML file itself as well as all the parameters used during the 
analysis leading to the mzIdentML file. Information regarding how the LC-MS 
experiment was performed should be collected from the raw data file. As the 
parameters used in different software solutions can vary greatly, all these
parameters are stored in a named list, which can thus be very different from 
pipeline to pipeline. It is the users responsibility to check conformity 
between samples.
}
\section{Methods (by generic)}{
\itemize{
\item \code{show}: Short summary of the content

\item \code{length}: Get the length of the object

\item \code{parameters}: Get the parameters used for the search

\item \code{software}: Get the software used to arrive at the results

\item \code{files}: Get the data files used for the analysis
}}
\section{Slots}{

\describe{
\item{\code{software}}{A data frame with information retaining to the software used 
for the analysis. At least the name and an id is given, but optionally also 
version number and URI.}

\item{\code{rawFile}}{A data frame with information about the raw data file(s) used 
for the analysis. The data frame will contain at least the location and 
spectrum ID format.}

\item{\code{databaseFile}}{A data frame containing at least the location and file 
format of the database file used in the search.}

\item{\code{idFile}}{A character string containing the location of the mzIdentML file
at the time of parsing.}

\item{\code{parameters}}{A list containing containing the information stored in the 
MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol node. 
SearchType and Threshold are the only required parameters given by the 
mzIdentML standard.}
}}
\section{Objects from the class}{

Objects of mzIDparameters are not meant to be created explicitly but as part 
of the \code{\link{mzID-class}}. Still object can be created with the 
constructor \code{\link{mzIDparameters}} (not exported).
}
\seealso{
\code{\link{mzIDparameters}}

Other mzID.classes: \code{\link{mzID-class}},
  \code{\link{mzIDCollection-class}},
  \code{\link{mzIDdatabase-class}},
  \code{\link{mzIDevidence-class}},
  \code{\link{mzIDpeptides-class}},
  \code{\link{mzIDpsm-class}}
}

