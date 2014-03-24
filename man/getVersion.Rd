\name{getVersion}
\alias{getVersion}
\title{Get the mzIdentML version and check that the version is supported}
\usage{
getVersion(ns)
}
\arguments{
  \item{ns}{The namespace of the mzIdentML file}
}
\value{
A textstring giving the version of the mzIdentML file if it
is supported. In case of missing support it throws an
error.
}
\description{
Currently version 1.1 and 1.0 are supported.
}

