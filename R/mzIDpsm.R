#' @include generics.R
#' @include aaa.R
NULL

#' A class to store psm information from an mzIdentML file
#' 
#' This class handles parsing and storage of scan info and the related psm's. 
#' This information resides in the
#' /*/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult
#' node.
#' 
#' The content of the class is stored as two data frames: One containing a row 
#' for each scan in the results, and one containing all psm's in the results. 
#' Additionally a list containing indexing from scan to psm is stored.
#' 
#' @section Objects from the class:
#' Objects of mzIDpsm are not meant to be created explicitly but as part of the 
#' \code{\link{mzID-class}}. Still object can be created with the constructor 
#' \code{\link{mzIDpsm}}.
#' 
#' 
#' @slot scans A data.frame containing all reference to all scans with at least 
#' one psm. The columns gives at least an ID, a spectrumID and a reference to 
#' the file used.
#' 
#' @slot id A data.frame containing all psm's from the analysis. The columns 
#' depend on the file but at least id, chargeState, experimentalMassToCharge, 
#' passThreshold and rank must exist according to the mzIdentML specifications.
#' 
#' @slot mapping A list with an entry for each row in @@scans. Each entry 
#' contains an integer vector pointing to the related rows in @@id.
#' 
#' @family mzID-classes
#' @seealso \code{\link{mzIDpsm}}
#' 
setClass(
    'mzIDpsm',
    slots=list(
        scans='data.frame',
        id='data.frame',
        mapping='list'
    ),
    validity=function(object){
        if(nrow(object@scans) != length(object@mapping) | sum(sapply(object@mapping, length)) != nrow(object@id)){
            return('Dimensions must match between elements')
        }
    },
    prototype=prototype(
        scans=data.frame(),
        id=data.frame(),
        mapping=list()
    )
)

#' @describeIn mzIDpsm A short summary of the content
#' 
#' @param object An mzIDpsm object
#' 
setMethod(
    'show', 'mzIDpsm',
    function(object){
        if(length(object) == 0){
            cat('An empty mzIDpsm object\n')
        } else {
            cat('An mzIDpsm object with', nrow(object@scans), 'scans and', nrow(object@id), 'psm\'s\n')
        }
    }
)

#' @describeIn mzIDpsm Get the number of psm'
#' 
#' @param x An mzIDpsm object
#' 
setMethod(
    'length', 'mzIDpsm',
    function(x){
        nrow(x@id)
    }
)

#' @describeIn flatten Merge id and scans according to the mapping
#' 
setMethod(
    'flatten', 'mzIDpsm',
    function(object, safeNames=TRUE){
        cbind(scans(object, safeNames=safeNames)[rep(1:length(object@mapping), sapply(object@mapping, length))[match(1:nrow(object@id),unlist(object@mapping))],],id(object, safeNames=safeNames))
    }
)

#' @describeIn mzIDpsm Get the identification results
#' 
#' @param safeNames Should column names be lowercased to ensure compatibility
#' between v1.0 and v1.1 files?
#' 
setMethod(
    'id', 'mzIDpsm',
    function(object, safeNames=TRUE){
        if(safeNames) {
            colNamesToLower(object@id)
        } else {
            object@id
        }
    }
)
#' @describeIn mzIDpsm Get the scans matched to peptides
#' 
#' @importFrom ProtGenerics scans
#' 
setMethod(
    'scans', 'mzIDpsm',
    function(object, safeNames=TRUE){
        if(safeNames) {
            colNamesToLower(object@scans)
        } else {
            object@scans
        }
    }
)
#' @describeIn mzIDpsm Get the link between scans and identifications
#' 
setMethod(
    'idScanMap', 'mzIDpsm',
    function(object){
        object@mapping
    }
)

#' A constructor for the mzIDpsm class
#' 
#' This function handles parsing of data and construction of an mzIDpsm object. 
#' This function is not intended to be called explicitly but as part of an mzID 
#' construction. Thus, the function is not exported.
#' 
#' @param doc an \code{XMLInternalDocument} created using 
#' \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector 
#' with the namespace named x.
#' 
#' @param addFinalizer \code{Logical} Sets whether reference counting should be 
#' turned on.
#' 
#' @param path If doc is missing the file specified here will be parsed.
#' 
#' @return An \code{mzIDpsm} object
#' 
#' @seealso \code{\link{mzIDpsm-class}}
#' @export
#' 
mzIDpsm <-function(doc, ns, addFinalizer=FALSE, path) {
    if (missing(doc)) {
        if (missing(path)) {
            return(new(Class = 'mzIDpsm'))
        } else {
            xml <- prepareXML(path)
            doc <- xml$doc
            ns <- xml$ns
        }
    }
    .path <- getPath(ns)
    scans <-
        attrExtract(doc, ns,
                    path=paste0(.path,
                                "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult"),
                    addFinalizer=addFinalizer)
    extra <- 
       attrExtractNameValuePair(doc, ns, 
                                path = paste0(.path, "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult"), 
                                c("cvParam", "userParam"), 
                                addFinalizer = TRUE)
    if(!is.null(extra)) {
        scans <- cbind(scans, extra)
    }
    if (nrow(scans) == 0) {
        return(new("mzIDpsm"))
    }
    
    ## create mzR compatible acquisitionNum column
    scans$acquisitionNum <- as.numeric(sub("^.*=([[:digit:]]+)$", "\\1", scans$spectrumID))

    id <- attrExtract(doc, ns,
                    path=paste0(.path, "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem"),
                    addFinalizer=addFinalizer)
    idParam <- attrExtractNameValuePair(doc, ns,
                                 path=paste0(.path, '/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem'),
                                 c('cvParam', 'userParam'),
                                 addFinalizer=addFinalizer)
    if (!is.null(idParam)) {
        id <- cbind(id, idParam)
    } else {}
    nID <-
        countChildren(doc, ns,
                      path=paste0(.path,
                                  "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult"),
                      'SpectrumIdentificationItem',
                      addFinalizer=addFinalizer)
    indMap <- list()
    indMap[nID > 0] <- split(1:nrow(id), rep(1:length(nID), nID))
    new(Class = 'mzIDpsm',
        scans = scans,
        id = id,
        mapping = indMap)
}
