\name{attrExtract}
\alias{attrExtract}
\title{Extract and properly format node attributes}
\usage{
attrExtract(doc, ns, path, child, addFinalizer = FALSE)
}
\arguments{
  \item{doc}{an \code{XMLInternalDocument} created using
  \code{\link[XML]{xmlInternalTreeParse}}}

  \item{ns}{A string with name \code{x} givin the namespace
  for the \code{doc}}

  \item{path}{An xpath expression giving the nodes to
  search in}

  \item{child}{A character vector giving the name of the
  children to search for}

  \item{addFinalizer}{\code{Logical} Sets whether reference
  counting should be turned on}
}
\value{
If \code{length(child) == 1} a data.frame with a column for
each attribute name and rows giving the value in each node.
If \code{length(child) > 1} a named list containing a
data.frame for each child. NA values are inserted if an
attribute is missing from a node.
}
\description{
This is a function to mainly handle correct formatting of
the output from \code{\link[XML]{xmlAttrs}}. In addition it
allows for multiple named child nodes to be specified.
}

