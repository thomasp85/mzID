#' @include generics.R
#' @include aaa.R
NULL

#' A class to store peptide evidence information from an mzIdentML file
#' 
#' This class handles parsing and storage of peptide evidence information from mzIDentML files, residing at the
#' /*/x:SequenceCollection/x:PeptideEvidence node.
#' 
#' The content of the class is stored in a data.frame with columns depending on the content of the mzIdentML
#' file. Columns represent the attribute values of for each PeptideEvidence node. For files conforming to the HUPO
#' standard, dbSequence_ref, id and peptide_ref is required while start, end, pre, post, name, isDecoy, frame and
#' translationTable_ref are optional. Information residing in cvParam and userParam children are not parsed.
#' 
#' @name mzIDevidence-class
#' 
#' @section Objects from the class:
#' Objects of mzIDevidence are not meant to be created explicitly but as part of the \code{\link{mzID-class}}. Still
#' object can be created with the constructor \code{\link{mzIDevidence}} (not exported).
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{evidence}:}{A data.frame containing all peptide evidence from the mzIdentML file}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{length}:}{Reports the number of peptide evidences}
#' }
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDevidence}}
#' 
#' @rdname mzIDevidence-class
#' 
setClass(
    'mzIDevidence',
    representation=representation(
        evidence='data.frame'
    ),
    prototype=prototype(
        evidence=data.frame()
    )
)

#' Show method for mzIDevidence objects
#' 
#' This function reports general information on the mzIDevidence object. It is called automatically when an object is querried.
#' 
#' @param object An mzIDevidence object
#' 
#' @return A description of the content of the mzIDevidence object
#' 
#' @seealso \code{\link{mzIDevidence-class}}
#' 
setMethod(
    'show', 'mzIDevidence',
    function(object){
        if(length(object) == 0){
            cat('An empty mzIDevidence object\n')
        } else {
            cat('An mzIDevidence object with', length(object), 'entries\n')
        }
    }
)

#' Report the length of an mzIDevidence object
#' 
#' The length of an mzIDevidence object is defined as the number of peptide evidences. An empty object has a length of 0
#' 
#' @param x An mzIDevidence object
#' 
#' @return A \code{numeric} giving the number of peptide evidences in the mzIDevidence object
#' 
#' @seealso \code{\link{mzIDevidence-class}}
#' 
setMethod(
    'length', 'mzIDevidence',
    function(x){
        nrow(x@evidence)
    }
)

#' A constructor for the mzIDevidence class
#' 
#' This function handles parsing of data and construction of an mzIDevidence object. This function is not intended to be called
#' explicitly but as part of an mzID construction. Thus, the function is not exported.
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#'
#' @param addFinalizer \code{Logical} Sets whether reference counting should be turned on
#' 
#' @param path If doc is missing the file specified here will be parsed
#' 
#' @return An \code{mzIDevidence} object
#' 
#' @seealso \code{\link{mzIDevidence-class}}
#' @export
#' 
mzIDevidence <- function(doc, ns, addFinalizer=FALSE, path) {
    if (missing(doc)) {
        if (missing(path)) {
            return(new(Class = 'mzIDevidence'))
        } else {
            xml <- prepareXML(path)
            doc <- xml$doc
            ns <- xml$ns
        }
    }
    .version <- getVersion(ns)
    .path <- getPath(ns)
    if (.version == "1.1") { 
        evidence <- attrExtract(doc, ns,
                                paste0(.path, '/x:SequenceCollection/x:PeptideEvidence'),
                                addFinalizer=addFinalizer)
    } else { ## "1.0"
        evidence <- attrExtract(doc, ns,
                                paste0(.path, '/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem/x:PeptideEvidence'),
                                addFinalizer=addFinalizer)
        evidence$peptide_ref <-
            sub("PE", "peptide",
                substr(evidence$id, 1, 6))
    }        
    new(Class = 'mzIDevidence',
        evidence = colNamesToLower(evidence))
}
