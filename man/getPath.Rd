\name{getPath}
\alias{getPath}
\title{Get the correct namespace path depending on version}
\usage{
getPath(ns)
}
\arguments{
  \item{ns}{The namespace of the mzIdentML file}
}
\value{
A textstring with the top parent node of the DOM
}
\description{
This function resolves an inconsistancy between version 1.0
and 1.1 where the namespace is changed in case.
}

