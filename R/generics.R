#' Flatten an mzID related class into a table
#' 
#' This function flattens the content of the object into a table by merging the content intelligently (it knows the links
#' between the different objects).
#' 
#' @param object The object to be flattened
#' 
#' @param safeNames Logical. Should column names be lowered to ensure compitability between different versions of the mzIdentML schema. Defaults to TRUE
#' 
#' @return A \code{data.frame} with the flattened result
#' 
#' @seealso \code{\link{mzID-class}}
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
#' @param safeNames Logical. Should column names be lowered to ensure compitability between different versions of the mzIdentML schema. Defaults to TRUE
#' 
#' @return A data frame or a list of data frames in the case of mzIDCollections
#' 
#' @rdname mzID-getters
#' @name mzID-getters
#' @docType methods
#' 
NULL

#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'database',
    def=function(object, safeNames=TRUE){standardGeneric('database')}
)
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
    'peptides',
    def=function(object, safeNames=TRUE){standardGeneric('peptides')}
)
#' @rdname mzID-getters
#' 
#' @export
#' 
setGeneric(
    'modifications',
    def=function(object){standardGeneric('modifications')}
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
    'scans',
    def=function(object, safeNames=TRUE){standardGeneric('scans')}
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
#' keyFor returns the internal hash for a given name.
#' 
#' @noRd
#' 
setGeneric(
    'increment', 
    def=function(object){standardGeneric('increment')}
)
setGeneric(
    'keyFor', 
    def=function(object, name){standardGeneric('keyFor')}
)