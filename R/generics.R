#' Flatten an mzID related class into a table
#' 
#' This function flattens the content of the object into a table by merging the 
#' content intelligently (it knows the links between the different objects).
#' 
#' @param object The object to be flattened
#' 
#' @param safeNames Logical. Should column names be lowered to ensure 
#' compitability between different versions of the mzIdentML schema. 
#' Defaults to TRUE
#' 
#' @return A \code{data.frame} with the flattened result or a list of 
#' data.frames
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDCollection-class}} 
#' \code{\link{mzIDpsm}} \code{\link{mzIDpeptides}}
#' 
#' @export
#' 
#' @examples
#' mzResults <- mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_tandem.mzid")
#' head(flatten(mzResults))
#' 
setGeneric(
    'flatten', 
    def=function(object, safeNames=TRUE){standardGeneric('flatten')}
)

#' Remove decoy identification
#' 
#' This function trims down an mzID or mzIDCollection object by removing all
#' information that is only related to the decoy database search. If some 
#' information relates to both the regular and decoy database (e.g. a peptide
#' sequence that can be found in both databases) it is kept.
#' 
#' @param object An mzID or mzIDCollection to remove decoy information from
#' 
#' @param ... Currently ignored
#' 
#' @return An mzID or mzIDCollection object depending on the input
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDCollection-class}}
#' 
#' @export
#' 
setGeneric(
    'removeDecoy', 
    def=function(object, ...){standardGeneric('removeDecoy')}
)

#' Getter functions for identification data
#' 
#' This set of functions are used to extract data from mzID and mzIDCollection
#' objects.
#' 
#' @param object An mzID or mzIDCollection object
#' 
#' @param safeNames Logical. Should column names be lowered to ensure 
#' compitability between different versions of the mzIdentML schema. Defaults to
#' TRUE
#' 
#' @return A data frame or a list of data frames in the case of mzIDCollections
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDCollection-class}}
#' 
#' @rdname mzID-getters
#' @name mzID-getters
#' 
NULL

#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'evidence',
    def=function(object, safeNames=TRUE){standardGeneric('evidence')}
)
#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'id',
    def=function(object, safeNames=TRUE){standardGeneric('id')}
)
#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'idScanMap',
    def=function(object){standardGeneric('idScanMap')}
)
#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'parameters',
    def=function(object){standardGeneric('parameters')}
)
#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'software',
    def=function(object){standardGeneric('software')}
)
#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'files',
    def=function(object){standardGeneric('files')}
)

## mzIDCollection INTERNALS
###########################
#' Tools to handle generation of lookup names for the dictionary
#' 
#' increment looks for a counter in the data environment of mzIDCollection
#' objects and increments it by one if it exists. Otherwise it initialises the
#' counter to 1. It returns the value of the counter.
#' 
#' @param object An mzIDCollection object
#' 
#' @return An integer given the current count
#' 
#' @keywords internal
#' 
setGeneric(
    'increment', 
    def=function(object){standardGeneric('increment')}
)
#' Tools to handle generation of lookup names for the dictionary
#' 
#' keyFor returns the internal hash for a given name.
#' 
#' @param object An mzIDCollection object
#' 
#' @return The hash under wich the object with the given name is stored
#' 
#' @keywords internal
#' 
setGeneric(
    'keyFor', 
    def=function(object, name){standardGeneric('keyFor')}
)