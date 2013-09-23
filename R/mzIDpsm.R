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
    representation=representation(
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
#' @aliases length,mzIDpsm-method
#' 
setMethod(
    'length', 'mzIDpsm',
    function(x){
        nrow(x@id)
    }
)

#' @rdname flatten-methods
#' @aliases flatten,mzIDpsm,ANY-method
#' @aliases flatten,mzIDpsm-method
#' 
setMethod(
    'flatten', 'mzIDpsm',
    function(object){
        cbind(object@scans[rep(1:length(object@mapping), sapply(object@mapping, length)),],object@id)
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
#' @return An \code{mzIDpsm} object
#' 
#' @seealso \code{\link{mzIDpsm-class}}
#' 
mzIDpsm <-function(doc, ns) {
    if (missing(doc)) {
        new(Class='mzIDpsm')
    } else {
        .path <- getPath(ns)
        scans <-
            attrExtract(doc, ns,
                        path=paste0(.path,
                                    "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult"))
        id <-
            attrExtract(doc, ns,
                        path=paste0(.path, "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem"))
        idParam <-
            attrExtractNameValuePair(doc, ns,
                                     path=paste0(.path, '/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem'),
                                     c('cvParam', 'userParam'))
        if (!is.null(idParam)) {
            id <- cbind(id, idParam)
        } 
        nID <-
            countChildren(doc, ns,
                          path=paste0(.path,
                                      "/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult"), 'SpectrumIdentificationItem')
        if (nID == 0)
            return(new("mzIDpsm"))
        indMap <- list()
        indMap[nID > 0] <- split(1:nrow(id), rep(1:length(nID), nID))
        new(Class='mzIDpsm', scans=scans, id=id, mapping=indMap)
    }
}
