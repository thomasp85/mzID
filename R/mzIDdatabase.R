#' @include generics.R
NULL

setClass(
  'mzIDdatabase',
  representation=representation(
    database='data.frame'
    ),
  prototype=prototype(
    database=data.frame()
    )
  )
setMethod(
  'show', 'mzIDdatabase',
  function(object){
    if(length(object) == 0){
      cat('An empty mzIDdatabase object\n')
    } else {
      cat('An mzIDdatabase object with', length(object), 'entries\n')
    }
  }
  )
setMethod(
  'length', 'mzIDdatabase',
  function(x){
    nrow(x@database)
  }
  )
mzIDdatabase <- function(doc, ns){
  if(missing(doc)){
    new(Class='mzIDdatabase')
  } else {
    database <- t(xpathSApply(doc, '/x:MzIdentML/x:SequenceCollection/x:DBSequence', namespaces=ns, fun=xmlAttrs))
    database <- do.call('data.frame', list(lapply(data.frame(database, stringsAsFactors=FALSE), type.convert, as.is=T), stringsAsFactors=FALSE))
    dbnames <- t(xpathSApply(doc, '/x:MzIdentML/x:SequenceCollection/x:DBSequence/x:cvParam', namespaces=ns, fun=xmlAttrs))[,'value']
    hasName <- as.logical(xpathSApply(doc, '/x:MzIdentML/x:SequenceCollection/x:DBSequence', namespaces=ns, fun=xmlSize))
    dbnames <- mapply(sub, paste('^\\Q',database$accession[hasName], '\\E', ' ', sep=''), '', dbnames)
    database$description <- NA
    database$description[hasName] <- dbnames
    new(Class='mzIDdatabase', database=database)
  }
}