% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/mzIDCollection.R
\docType{methods}
\name{keyFor}
\alias{keyFor}
\alias{keyFor,mzIDCollection,character-method}
\title{Tools to handle generation of lookup names for the dictionary}
\usage{
keyFor(object, name)

\S4method{keyFor}{mzIDCollection,character}(object, name)
}
\arguments{
\item{object}{An mzIDCollection object}
}
\value{
The hash under wich the object with the given name is stored
}
\description{
keyFor returns the internal hash for a given name.
}
\section{Methods (by class)}{
\itemize{
\item \code{object = mzIDCollection,name = character}: Get the key for sample in an mzIDCollection object
}}
\keyword{internal}

