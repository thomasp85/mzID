#' @include mzID.R
#' @include generics.R
#' @include aaa.R
NULL

#' A class to handle a set of mzID objects
#' 
#' This class is a container for multiple mzID objects. It is constructed such
#' that the bulk data are not copied when passed around. It is the aim that this
#' class have parity with the mzID class in the methods it exposes to the user, 
#' such that mzIDCollections can be thought of as vectors in the traditional R
#' sense. Furthermore it accepts standard indexing and concatenation.
#' 
#' Objects of this class is usually constructed be passing mulitple files to the
#' mzID constructor, or by combining mulitple mzID objects.
#' 
#' @section Methods:
#' mzIDCollections support the same methods as mzID object but will return the
#' results for each mzID object as an entry in a list. Apart from this
#' mzIDCollections support standard vector indexing and concatenation as 
#' described in \code{\link{mzIDCollectionUtilities}}
#' 
#' @name mzIDCollection-class
#' 
#' @exportClass mzIDCollection
#' @rdname mzIDCollection-class
#' 
setClass('mzIDCollection',
         slots = list(
             data = 'environment',
             .lookup = 'matrix'
         ),
         validity = function(object) {
             if(!all(sapply(object@data, function(x) {class(x) == 'mzID'}))) {
                 return('All elements must be mzID objects')
             }
             if(length(object) != 0 && length(object) != length(ls(object@data))) {
                 return('Number of mzID objects must match the rows in the lookup table')
             }
             TRUE
         },
         prototype = prototype(
             data = new.env(),
             .lookup = matrix(nrow=0, ncol=2, dimnames=list(NULL, c('sampleName', 'key')))
         )
)

#' Show method for mzIDCollection objects
#' 
#' This function gives a very brief overview of the content of an mzIDCollection
#' object.
#' 
#' @param object An mzIDCollection object
#' 
#' @return NULL
#' 
#' @seealso \code{\link{mzIDCollection-class}}
#' 
#' @noRd
#' 
setMethod('show', 'mzIDCollection',
          function(object) {
              if(length(object)) {
                  cat('An mzIDCollection object containing', length(object), 'samples')
              } else {
                  cat('An empty mzIDCollection object\n')
              }
          }
)

#' Get the length of an mzIDCollection object
#' 
#' The length of an mzIDCollection object is defined as the number of mzID 
#' objects it contain.
#' 
#' @param x An mzIDCollection object
#' 
#' @return An integer giving the number of mzID objects contained in the 
#' mzIDCollection
#' 
#' @seealso \code{\link{mzIDCollection-class}}
#' 
#' @noRd
#' 
setMethod('length', 'mzIDCollection',
          function(x) {
              nrow(x@.lookup)
          }
)

#' Convert an mzIDCollection to a list of mzID object
#' 
#' @noRd
#' 
setAs('mzIDCollection', 'list',
      function(from) {
          theList <- lapply(names(from), function(x) {from@data[[keyFor(from, x)]]})
          names(theList) <- names(from)
          theList
      }
    
)
as.list.mzIDCollection <- function(object) {as(object, 'list')}

#' see mzIDCollection internals in generics.R
#' 
#' @noRd
#' 
setMethod('increment', 'mzIDCollection',
          function(object) {
              if(exists('.counter', object@data)) {
                  object@data$.counter <- object@data$.counter + 1
              } else {
                  assign('.counter', 1, object@data)
              }
              object@data$.counter
          }
)
setMethod('keyFor', c('mzIDCollection', 'character'),
          function(object, name) {
              as.vector(object@.lookup[names(object) == name, 'key'])
          }
)

#' see flatten
#' 
#' @importFrom plyr rbind.fill
#' 
#' @noRd
#' 
setMethod('flatten', 'mzIDCollection',
          function(object, safeNames=TRUE) {
              rbind.fill(lapply(as.list(object), flatten, safeNames=safeNames))
          }
)

#' see removeDecoy
#' 
#' @noRd
#' 
setMethod('removeDecoy', 'mzIDCollection',
          function(object) {
              for(i in names(object)) {
                  mzid <- object[i]
                  object@data[[keyFor(object, i)]] <- removeDecoy(mzid)
              }
              object
          }
)

#' Create a new mzIDCollection
#' 
#' This function creates a new mzIDCollection object containing the supplied 
#' mzID object. As such the result is equivalent to passing a number of mzID
#' objects to \code{c()}, except that an empty mzIDCollection object is returned
#' if no mzID objects are supplied.
#' 
#' @param ... An arbitrary number of mzID objects
#' 
#' @return An mzIDCollection object
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDCollection-class}}
#' 
#' @export
#' 
mzIDCollection <- function(...) {
    collection <- new(Class='mzIDCollection', data=new.env())
    data <- eval(substitute(alist(...)))
    if(length(data) != 0) {
        for(i in 1:length(data)) {
            obj <- data[[i]]
            collection <- c(collection, eval.parent(obj))
        }
    }
    collection
}
#' Utility functions for mzIDCollection object
#' 
#' These functions provide list-like handling of mzIDCollection object. Their 
#' behavior are fully in line with that of standard R lists.
#' 
#' @param x An mzIDCollection object
#' 
#' @rdname mzIDCollectionUtilities
#' @docType methods
#' @name Utilities
#' 
setMethod('names', 'mzIDCollection',
          function(x) {
              x@.lookup[,1]
          }
)

#' @rdname mzIDCollectionUtilities
#' 
#' @usage \S4method{names<-}{mzIDCollection}(x, value)
#' 
#' @param value A string giving the name(s) to set
#' 
#' @name Utilities
#' @docType methods
#' 
setReplaceMethod('names', c('mzIDCollection', 'character'),
                 function(x, value) {
                     x@.lookup[, 1] <- value
                     x
                 }
)

#' @rdname mzIDCollectionUtilities
#' 
#' @param i An integer or a string giving the index or the name respectively
#' @param j ignored
#' @param ... ignored
#' 
setMethod('[[', c('mzIDCollection', 'numeric', 'missing'),
          function(x, i, j, ...) {
              if(length(i) != 1 || i <= 0 || i > length(x)) stop('subscript out of bounds')
              
              x[[names(x)[i]]]
          }
)

#' @rdname mzIDCollectionUtilities
#' 
setMethod('[[', c('mzIDCollection', 'character', 'missing'),
          function(x, i, j, ...) {
              if(length(i) != 1) stop('subscript out of bounds')
              if(!i %in% names(x)) return(NULL)
              
              x@data[[keyFor(x, i)]]
          }
)

#' @rdname mzIDCollectionUtilities
#' 
#' @param y An mzID or mzIDCollection object
setMethod('c', 'mzID', 
          function(x, y, ..., recursive=FALSE) {
              if(missing(y)) {
                  mzIDCollection(x)
              } else if(class(y) == 'mzID') {
                  x <- mzIDCollection(x, y)
                  c(x, ...)
              } else if(class(y) == 'mzIDCollection') {
                  x <- c(y, x)
                  x@.lookup <- x@.lookup[c(length(x), 1:(length(x)-1)),]
                  c(x, ...)
              } else {
                  stop('Cannot combine mzID with objects of type', class(y))
              }
          }
)

#' @rdname mzIDCollectionUtilities
#' 
setMethod('c', 'mzIDCollection', 
          function(x, y, ..., recursive=FALSE) {
              if(missing(y)) {
                  x
              } else if(class(y) == 'mzID') {
                  sname <- sub("\\.[^.]*$", "", basename(y@parameters@idFile))
                  varName <- paste0('key', increment(x))
                  x@.lookup <- rbind(x@.lookup, c(sname, varName))
                  assign(varName, y, pos=x@data)
                  c(x, ...)
              } else if(class(y) == 'mzIDCollection') {
                  for(i in names(y)) {
                      x <- c(x, y[[i]])
                  }
                  c(x, ...)
              } else {
                  stop('Cannot combine mzIDCollection with objects of type', class(y))
              }
              
          }
)

## GETTER FUNCTIONS
###################

#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'database', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), database, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'evidence', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), evidence, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'parameters', 'mzIDCollection',
    function(object){
        lapply(as.list(object), parameters)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'software', 'mzIDCollection',
    function(object){
        lapply(as.list(object), software)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'files', 'mzIDCollection',
    function(object){
        lapply(as.list(object), files)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'peptides', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), peptides, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'modifications', 'mzIDCollection',
    function(object){
        lapply(as.list(object), modifications)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'id', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), id, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'scans', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), scans, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'idScanMap', 'mzIDCollection',
    function(object){
        lapply(as.list(object), idScanMap)
    }
)