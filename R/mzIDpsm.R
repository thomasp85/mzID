#' @include generics.R
NULL

#' A Class
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
setMethod(
  'length', 'mzIDpsm',
  function(x){
    nrow(x@id)
  }
  )

mzIDpsm <-function(doc, ns){
  if(missing(doc)){
    new(Class='mzIDpsm')
  } else {
    scans <- data.frame(t(xpathSApply(doc, path="/x:MzIdentML/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult", namespaces=ns, fun=xmlAttrs)), stringsAsFactors=FALSE)
    id <- t(xpathSApply(doc, path="/x:MzIdentML/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem", namespaces=ns, fun=xmlAttrs))
    id <- do.call('data.frame', lapply(data.frame(id, stringsAsFactors=FALSE), type.convert, as.is=T))
    idParam <- t(xpathSApply(doc, path="/x:MzIdentML/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem/x:cvParam", namespaces=ns, fun=xmlAttrs))
    idParam <- do.call('data.frame', c(lapply(split(idParam[,'value'], idParam[,'name']), type.convert, as.is=TRUE), stringsAsFactors=FALSE))
    userParam <- t(xpathSApply(doc, path="/x:MzIdentML/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult/x:SpectrumIdentificationItem/x:userParam", namespaces=ns, fun=xmlAttrs))
    userParam <- do.call('data.frame', c(lapply(split(userParam[,'value'], userParam[,'name']), type.convert, as.is=TRUE), stringsAsFactors=FALSE))
    idParam <- cbind(id, idParam, userParam)
    nID <- xpathSApply(doc, path="/x:MzIdentML/x:DataCollection/x:AnalysisData/x:SpectrumIdentificationList/x:SpectrumIdentificationResult", namespaces=ns, fun=xmlSize)
    indMap <- list()
    indMap[nID > 0] <- split(1:nrow(idParam), rep(1:length(nID), nID))
    new(Class='mzIDpsm', scans=scans, id=idParam, mapping=indMap)
  }
}