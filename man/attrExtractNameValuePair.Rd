\name{attrExtractNameValuePair}
\alias{attrExtractNameValuePair}
\title{Extract and format attributes from nodes with name and value attributes}
\usage{
attrExtractNameValuePair(doc, ns, path, child, addFinalizer = FALSE)
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
A data.frame with columns for each unique value in the name
attribute of the nodes search in. The data.frame will have
rows for each node in the path expression, regardless of
whether it contains children.
}
\description{
This function handles the special case of extracting
information from nodes with the attributes name and value.
This function treats the value of the name attribute as the
category for the value of the value attribute, so that it
in essence equals an XML node of the form \code{<node
category='value'/>}. Several child names can be specified
and the result will in the end be merged to a single
data.frame rows for each node in the path argument.
}

