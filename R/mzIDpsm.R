#' @include generics.R
#' @include aaa.R
NULL

#' A class to store psm information from an mzIdentML file
#' 
#' This class handles parsing and storage of scan info and the related psm's. This information resides in the
#' /*/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult node.
#' 
#' The content of the class is stored as two data frames: One containing a row for each scan in the results, and one
#' containing all psm's in the results. Additionally a list containing indexing from scan to psm is stored.
#' 
#' @name mzIDpsm-class
#' 
#' @section Objects from the class:
#' Objects of mzIDpsm are not meant to be created explicitly but as part of the \code{\link{mzID-class}}. Still
#' object can be created with the constructor \code{\link{mzIDpsm}} (not exported).
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{scans}:}{A data.frame containing all reference to all scans with at least one psm. The columns gives at least an ID, a spectrumID and a reference to the file used}
#'  \item{\code{id}:}{A data.frame containing all psm's from the analysis. The columns depend on the file but at least id, chargeState, experimentalMassToCharge, passThreshold and rank must exist according to the mzIdentML specifications}
#'  \item{\code{mapping}:}{A list with an entry for each row in @@scans. Each entry contains an integer vector pointing to the related rows in @@id}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{length}:}{Reports the number of psm's}
#' }
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDpsm}}
#' 
#' @rdname mzIDpsm-class
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
            stop('Dimensions must match between elements')
        }
    },
    prototype=prototype(
        scans=data.frame(),
        id=data.frame(),
        mapping=list()
    )
)

#' Show method for mzIDpsm objects
#' 
#' This function reports general information on the mzIDpsm object. It is called automatically when an object is querried.
#' 
#' @param object An mzIDpsm object
#' 
#' @return A description of the content of the mzIDpsm object
#' 
#' @seealso \code{\link{mzIDpsm-class}}
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

#' Report the length of an mzIDpsm object
#' 
#' The length of an mzIDpsm object is defined as the number of psm's. An empty object has a length of 0
#' 
#' @param x An mzIDpsm object
#' 
#' @return A \code{numeric} giving the number of psm's in the mzIDpsm object
#' 
#' @seealso \code{\link{mzIDpsm-class}}
#' 
setMethod(
    'length', 'mzIDpsm',
    function(x){
        nrow(x@id)
    }
)

#' @rdname flatten-methods
#' 
setMethod(
    'flatten', 'mzIDpsm',
    function(object){
        cbind(object@scans[rep(1:length(object@mapping), sapply(object@mapping, length))[match(1:nrow(object@id),unlist(object@mapping))],],object@id)
    }
)

#' A constructor for the mzIDpsm class
#' 
#' This function handles parsing of data and construction of an mzIDpsm object. This function is not intended to be called
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
        scans = colNamesToLower(scans),
        id = colNamesToLower(id),
        mapping = indMap)
}
