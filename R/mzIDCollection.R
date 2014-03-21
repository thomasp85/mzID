#' @include mzID.R
#' @include generics.R
#' @include aaa.R
NULL

#' @exportClass mzIDCollection
#' 
setClass('mzIDCollection',
         representation = representation(
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

setMethod('show', 'mzIDCollection',
          function(object) {
              if(length(object)) {
                  cat('An mzIDCollection object containing', length(object), 'samples')
              } else {
                  cat('An empty mzIDCollection object\n')
              }
          }
)

setMethod('length', 'mzIDCollection',
          function(x) {
              nrow(x@.lookup)
          }
)
setAs('mzIDCollection', 'list',
      function(from) {
          theList <- lapply(names(from), function(x) {from@data[[keyFor(from, x)]]})
          names(theList) <- names(from)
          theList
      }
    
)
as.list.mzIDCollection <- function(object) {as(object, 'list')}

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
setMethod('names', 'mzIDCollection',
          function(x) {
              x@.lookup[,1]
          }
)
setReplaceMethod('names', c('mzIDCollection', 'character'),
                 function(x, value) {
                     x@.lookup[, 1] <- value
                     x
                 }
)
setMethod('[[', c('mzIDCollection', 'numeric', 'missing'),
          function(x, i, j, ...) {
              if(length(i) != 1 || i <= 0 || i > length(x)) stop('subscript out of bounds')
              
              x[[names(x)[i]]]
          }
)
setMethod('[[', c('mzIDCollection', 'character', 'missing'),
          function(x, i, j, ...) {
              if(length(i) != 1) stop('subscript out of bounds')
              if(!i %in% names(x)) return(NULL)
              
              x@data[[keyFor(x, i)]]
          }
)

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

#' @importFrom plyr rbind.fill
setMethod('flatten', 'mzIDCollection',
          function(object, no.redundancy=FALSE) {
              rbind.fill(lapply(as.list(object), flatten, no.redundancy=no.redundancy))
          }
)

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