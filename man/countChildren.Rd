\name{countChildren}
\alias{countChildren}
\title{Report the number of children with specific name per node in an XML document}
\usage{
countChildren(doc, ns, path, child, withPar, simplify = TRUE,
  addFinalizer = FALSE)
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

  \item{withPar}{(optional) A character string giving a
  name of an attribute that must exist in the child for it
  to be considred in counting.}

  \item{simplify}{(default=TRUE) A logical value giving
  whether the output should be returned as a dataframe if
  only one child is given}

  \item{addFinalizer}{\code{Logical} Sets whether reference
  counting should be turned on}
}
\value{
If \code{length(child) == 1} and \code{simplify == TRUE} a
vector giving the number of matches per node. If
\code{length(child) > 1} or \code{simplify == FALSE} a
named list with an element per child argument containing a
vector giving the number of matches per node.
}
\description{
This function takes an xpath argument and reports the
number of children with given names at each node. Several
children can be specified
}
\details{
Given an xmlInternalDocument, a namespace, an xpath
argument and a number of names this function will count,
for each node in the xpath argument, how many children with
the specified name(s) exists. It is optimized for several
child names, so that the performance hit with additional
names are neglectible.

An additional constraint on the match can be given with the
\code{withPar} argument, which requests that each child, in
addition to having the correct name, also should contain an
attribute with the name given in the \code{withPar}
argument, in order to be counted. This search requires
addition look- ups in the XML document and will thus give a
performance hit.

If our XML document had the following structure, and
\code{doc} and \code{ns} was set up correctly

\code{<node/>}\cr \code{__<child1 value='5'/>}\cr
\code{__<child1 value='1'/>}\cr \code{__<child2/>}\cr
\code{</node>}\cr \code{<node/>}\cr \code{__<child1/>}\cr
\code{__<child2/>}\cr \code{__<child2/>}\cr
\code{</node>}\cr\cr

\code{countChildren(doc, ns, path='./x:node',
child='child1')}

would return:\cr \code{[1] 2 1}

\code{countChildren(doc, ns, path='./x:node',
child='child1', withPar='value')}

would return:\cr \code{[1] 2 0}

\code{countChildren(doc, ns, path='./x:node',
child='child1', simplify=FALSE)}

would return:\cr \code{$child1}\cr \code{[1] 2 1}

\code{countChildren(doc, ns, path='./x:node',
child=c('child1', 'child2')}

would return:\cr \code{$child1}\cr \code{[1] 2 1}\cr\cr
\code{$child2}\cr \code{[1] 1 2}\cr\cr
}

