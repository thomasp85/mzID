\name{prepareXML}
\alias{prepareXML}
\title{Parses an xml file and defines the namespace}
\usage{
prepareXML(path, addFinalizer = FALSE)
}
\arguments{
  \item{path}{The path to the xml file}

  \item{addFinalizer}{\code{Logical} Sets whether reference
  counting should be turned on}
}
\value{
A list with the named elements: doc: the results of
xmlInternalTreeParse and ns: the namespace as a vector.
}
\description{
Parses an xml file and defines the namespace
}

