#' Flatten an mzID related class into a table
#' 
#' This function flattens the content of the object into a table by merging the content intelligently (it knows the links
#' between the different objects).
#' 
#' @param object The object to be flattened
#' 
#' @param ... Additional parameters (currently unused)
#' 
#' @return A \code{data.frame} with the flattened result
#' 
#' @seealso \code{\link{mzID-class}}
#' 
#' @export
#' @docType methods
#' @rdname flatten-methods
#' 
#' @examples
#' mzResults <- mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_tandem.mzid")
#' head(flatten(mzResults))
#' 
setGeneric(
    'flatten', 
    def=function(object, ...){standardGeneric('flatten')}
)


setGeneric(
    'increment', 
    def=function(object){standardGeneric('increment')}
)
setGeneric(
    'keyFor', 
    def=function(object, name){standardGeneric('keyFor')}
)