#' @include generics.R
#' @include aaa.R
NULL

#' A class to store peptide evidence information from an mzIdentML file
#' 
#' This class handles parsing and storage of peptide evidence information from 
#' mzIDentML files, residing at the /*/x:SequenceCollection/x:PeptideEvidence 
#' node.
#' 
#' The content of the class is stored in a data.frame with columns depending on 
#' the content of the mzIdentML file. Columns represent the attribute values of 
#' for each PeptideEvidence node. For files conforming to the HUPO standard, 
#' dbSequence_ref, id and peptide_ref is required while start, end, pre, post, 
#' name, isDecoy, frame and translationTable_ref are optional. Information 
#' residing in cvParam and userParam children are not parsed.
#' 
#' @section Objects from the class:
#' Objects of mzIDevidence are not meant to be created explicitly but as part of
#' the \code{\link{mzID-class}}. Still object can be created with the 
#' constructor \code{\link{mzIDevidence}}.
#' 
#' 
#' @slot evidence A data.frame containing all peptide evidence from the 
#' mzIdentML file
#' 
#' @family mzID-classes
#' @seealso \code{\link{mzIDevidence}}
#' 
setClass(
    'mzIDevidence',
    slots=list(
        evidence='data.frame'
    ),
    prototype=prototype(
        evidence=data.frame()
    )
)

#' @describeIn mzIDevidence Short summary of the content of the object
#' 
#' @param object An mzIDevidence object
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

#' @describeIn mzIDevidence Report number of evidence
#' 
#' @param x An mzIDevidence object
#' 
setMethod(
    'length', 'mzIDevidence',
    function(x){
        nrow(x@evidence)
    }
)

#' @describeIn mzIDevidence Get the evidence from the peptide search
#' 
#' @param safeNames Should column names be lowercased to ensure compatibility
#' between v1.0 and v1.1 files?
#' 
setMethod(
    'evidence', 'mzIDevidence',
    function(object, safeNames=TRUE){
        if(safeNames) {
            colNamesToLower(object@evidence)
        } else {
            object@evidence
        }
    }
)

#' A constructor for the mzIDevidence class
#' 
#' This function handles parsing of data and construction of an mzIDevidence 
#' object. This function is not intended to be called explicitly but as part of 
#' an mzID construction. Thus, the function is not exported.
#' 
#' @param doc an \code{XMLInternalDocument} created using 
#' \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector 
#' with the namespace named x
#'
#' @param addFinalizer \code{Logical} Sets whether reference counting should be 
#' turned on
#' 
#' @param path If doc is missing the file specified here will be parsed
#' 
#' @return An \code{mzIDevidence} object
#' 
#' @seealso \code{\link{mzIDevidence-class}}
#' 
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
        evidence = evidence)
}
