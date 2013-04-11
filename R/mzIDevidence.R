#' @include generics.R
NULL

setClass(
  'mzIDevidence',
  representation=representation(
    evidence='data.frame'
    ),
  prototype=prototype(
    evidence=data.frame()
    )
  )
setMethod(
  'show', 'mzIDevidence',
  function(object){
    if(length(object) == 0){
      cat('An empty mzIDevidence object')
    } else {
      cat('An mzIDevidence object with', length(object), 'entries\n')
    }
  }
  )
setMethod(
  'length', 'mzIDevidence',
  function(x){
    nrow(x@evidence)
  }
  )
mzIDevidence <- function(doc, ns){
  if(missing(doc)){
    new(Class='mzIDevidence')
  } else {
    evidence <- t(xpathSApply(doc, '/x:MzIdentML/x:SequenceCollection/x:PeptideEvidence', namespaces=ns, fun=xmlAttrs))
    evidence <- do.call('data.frame', list(lapply(data.frame(evidence, stringsAsFactors=FALSE), type.convert, as.is=T), stringsAsFactors=FALSE))
    new(Class='mzIDevidence', evidence=evidence)
  }
}