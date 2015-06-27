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
#' 
#' @slot data An environment that holds the individual mzID objects
#' 
#' 
#' @slot .lookup A matrix with indexing information for retriving the mzID 
#' objects in the @@data slot.
#' 
#' @family mzID-classes
#' @seealso \code{\link{mzID}} \code{\link{mzIDCollection}}
#' @exportClass mzIDCollection
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

#' @describeIn mzIDCollection A short summary of the content of the object
#' 
#' @param object An mzIDCollection object
#' 
setMethod('show', 'mzIDCollection',
          function(object) {
              if(length(object)) {
                  cat('An mzIDCollection object containing', length(object), 'samples\n')
              } else {
                  cat('An empty mzIDCollection object\n')
              }
          }
)

#' @describeIn mzIDCollection Return the number of mzID object in the collection
#' 
#' @param x An mzIDCollection object
#' 
setMethod('length', 'mzIDCollection',
          function(x) {
              nrow(x@.lookup)
          }
)

#' @noRd
#' 
setAs('mzIDCollection', 'list',
      function(from) {
          theList <- lapply(names(from), function(x) {from@data[[keyFor(from, x)]]})
          names(theList) <- names(from)
          theList
      }
    
)
#' @rdname mzIDCollection-class
#' 
as.list.mzIDCollection <- function(object) {as(object, 'list')}

#' @describeIn increment Increment the counter in mzIDcollection objects
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
#' @describeIn keyFor Get the key for sample in an mzIDCollection object
#' 
setMethod('keyFor', c('mzIDCollection', 'character'),
          function(object, name) {
              as.vector(object@.lookup[names(object) == name, 'key'])
          }
)

#' @describeIn flatten Flatten all mzID object in the collection into a list of
#' data frames.
#' 
#' @importFrom plyr rbind.fill
#' 
setMethod('flatten', 'mzIDCollection',
          function(object, safeNames=TRUE) {
              rbind.fill(lapply(as.list(object), flatten, safeNames=safeNames))
          }
)

#' @describeIn mzIDCollection Removes decoys in all mzID object in collection
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
#' @describeIn mzIDCollection Get the names of the mzID object stored in the
#' collection
#' 
setMethod('names', 'mzIDCollection',
          function(x) {
              x@.lookup[,1]
          }
)

#' @describeIn mzIDCollection Set the names of the mzID object stored in the
#' collection
#' 
#' @param value A character vector of desired names
#' 
setMethod('names<-', c('mzIDCollection', 'character'),
                 function(x, value) {
                     x@.lookup[, 1] <- value
                     x
                 }
)

#' @describeIn mzIDCollection Extract an mzID object by index
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

#' @describeIn mzIDCollection Extract an mzID object by name
#' 
setMethod('[[', c('mzIDCollection', 'character', 'missing'),
          function(x, i, j, ...) {
              if(length(i) != 1) stop('subscript out of bounds')
              if(!i %in% names(x)) return(NULL)
              
              x@data[[keyFor(x, i)]]
          }
)

#' @describeIn mzIDCollection Subset collection by index
#' 
#' @param drop ignored
#' 
setMethod('[', c('mzIDCollection', 'numeric', 'missing', 'missing'),
          function(x, i, j, drop) {
              if(all(i <= 0)) {
                  i <- which(!1:length(x) %in% abs(i))
              } else if(any(i < 0)) {
                  stop('only 0\'s may be mixed with negative subscripts')
              }
              newLookup <- x@.lookup[i, ]
              new('mzIDCollection', data=x@data, .lookup=newLookup)
          }
)
#' @describeIn mzIDCollection Subset collection by name
#' 
setMethod('[', c('mzIDCollection', 'character', 'missing', 'missing'),
          function(x, i, j, drop) {
              i <- which(i %in% names(x))
              x[i]
          }
)
#' @describeIn mzIDCollection Subset collection by logical value
#' 
setMethod('[', c('mzIDCollection', 'logical', 'missing', 'missing'),
          function(x, i, j, drop) {
              i <- which(rep(i, length.out=length(x)))
              x[i]
          }
)

#' @describeIn mzID Combine mzID and mzIDCollection objects
#' 
#' @param y An mzID or mzIDCollection object
#' 
#' @param ... ignored
#' 
#' @param recursive ignored
#' 
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

#' @describeIn mzIDCollection Combine mzIDCollction and mzID objects
#' 
#' @param y An mzID or mzIDCollection object
#' 
#' @param recursive ignored
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

#' @describeIn mzIDCollection Get the database used for searching
#' 
#' @param safeNames Should column names be lowercased to ensure compatibility
#' between v1.0 and v1.1 files?
#' 
#' @importFrom ProtGenerics database
#' 
setMethod(
    'database', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), database, safeNames=safeNames)
    }
)
#' @describeIn mzIDCollection Get the evidence from the peptide search
#' 
setMethod(
    'evidence', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), evidence, safeNames=safeNames)
    }
)
#' @describeIn mzIDCollection Get the parameters used for the search
#' 
setMethod(
    'parameters', 'mzIDCollection',
    function(object){
        lapply(as.list(object), parameters)
    }
)
#' @describeIn mzIDCollection Get the software used to arrive at the results
#' 
setMethod(
    'software', 'mzIDCollection',
    function(object){
        lapply(as.list(object), software)
    }
)
#' @describeIn mzIDCollection Get the data files used for the analysis
#' 
setMethod(
    'files', 'mzIDCollection',
    function(object){
        lapply(as.list(object), files)
    }
)
#' @describeIn mzIDCollection Get the peptides identified.
#' 
#' @importFrom ProtGenerics peptides
#' 
setMethod(
    'peptides', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), peptides, safeNames=safeNames)
    }
)
#' @describeIn mzIDCollection Get the modification on the identified peptides
#' 
#' @importFrom ProtGenerics modifications
#' 
setMethod(
    'modifications', 'mzIDCollection',
    function(object){
        lapply(as.list(object), modifications)
    }
)
#' @describeIn mzIDCollection Get the identification results
#' 
setMethod(
    'id', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), id, safeNames=safeNames)
    }
)
#' @describeIn mzIDCollection Get the scans matched to peptides
#' 
#' @importFrom ProtGenerics scans
#' 
setMethod(
    'scans', 'mzIDCollection',
    function(object, safeNames=TRUE){
        lapply(as.list(object), scans, safeNames=safeNames)
    }
)
#' @describeIn mzIDCollection Get the link between scans and identifications
#' 
setMethod(
    'idScanMap', 'mzIDCollection',
    function(object){
        lapply(as.list(object), idScanMap)
    }
)