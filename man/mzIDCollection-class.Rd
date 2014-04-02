\docType{class}
\name{mzIDCollection-class}
\alias{mzIDCollection-class}
\title{A class to handle a set of mzID objects}
\description{
This class is a container for multiple mzID objects. It is
constructed such that the bulk data are not copied when
passed around. It is the aim that this class have parity
with the mzID class in the methods it exposes to the user,
such that mzIDCollections can be thought of as vectors in
the traditional R sense. Furthermore it accepts standard
indexing and concatenation.
}
\details{
Objects of this class is usually constructed be passing
mulitple files to the mzID constructor, or by combining
mulitple mzID objects.
}

