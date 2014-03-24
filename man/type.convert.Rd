\name{type.convert}
\alias{type.convert}
\title{An extension of type.convert() to handle XML like logicals (lower case)}
\usage{
type.convert(...)
}
\arguments{
  \item{...}{Parameters passed on to \code{type.convert}}
}
\description{
This function is a simple extension to the
\code{\link[utils]{type.convert}} function. It handles the
extra case where a vector of lower case true and false
value would not be converted to logicals. As this is the
standard format in XML files, this extension is needed
}
\seealso{
\code{\link[utils]{type.convert}}
}

