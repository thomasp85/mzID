#' @include generics.R
NULL

#' A Class
setClass(
  'mzIDparameters',
  representation=representation(
    software='data.frame',
    rawFile='data.frame',
    databaseFile='character',
    idFile='character',
    parameters='list'
    ),
  prototype=prototype(
    software=data.frame(),
    rawFile=data.frame(),
    databaseFile=character(),
    idFile=character(),
    parameters=list())
  )
setMethod(
  'show', 'mzIDparameters',
  function(object){
    if(length(object) == 0){
      cat('An empty mzIDparameters object\n')
    } else {
      cat('An mzIDparameters object:\n\n')
      cat('Software used:\t', object@software$name[1], ' (version: ', object@software$version[1], ')\n', sep='')
      if(nrow(object@software) > 1){
        for(i in 2:nrow(object@software)){
          cat('\t\t\t\t', object@software$name[i], ' (version ', object@software$version[i], ')\n', sep='')
        }
      } else {}
      if(nrow(object@rawFile) == 1){
        cat('\nRawfile:\t\t', object@rawFile$location[1], '\n', sep='')        
      } else {
        cat('\nRawfiles:\t\t', object@rawFile$location[1], '\n', sep='')
        for(i in 2:nrow(object@rawFile)){
          cat('\t\t\t\t', object@rawFile$location[i], '\n', sep='')
        }
      }
      cat('\nDatabase:\t\t', object@databaseFile, '\n', sep='')
    }
  }
  )
setMethod(
  'length', 'mzIDparameters',
  function(x){
    nrow(x@rawFile)
  }
  )

#' Parse parameter information from an mzIdentML file
#' 
#' Extracts the information specific to how the peptide identification was carried out.
#' 
#' @param file The path to the mzIdentML file
#' @param doc
mzIDparameters <- function(file, doc, ns){
  if(missing(doc)){
    new(Class='mzIDparameters')
  } else {
    software <- data.frame(t(xpathSApply(doc, '/x:MzIdentML/x:AnalysisSoftwareList/x:AnalysisSoftware', namespaces=ns, fun=xmlAttrs)), stringsAsFactors=FALSE)
    idFile <- file
    rawFile <- data.frame(t(xpathSApply(doc, '/x:MzIdentML/x:DataCollection/x:Inputs/x:SpectraData', namespaces=ns, fun=xmlAttrs)), stringsAsFactors=FALSE)
    databaseFile <- as.vector(unlist(getNodeSet(doc, '/x:MzIdentML/x:DataCollection/x:Inputs/x:SearchDatabase/@location', namespaces=ns)))
    userPar <- xpathSApply(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:AdditionalSearchParams/x:userParam', namespaces=ns, fun=xmlAttrs)
    parameters <- split(userPar['value', ], 1:length(userPar['value',]))
    names(parameters) <- userPar['name',]
    parameters <- lapply(parameters, type.convert)
    parameters$Enzymes <- data.frame(
      Name=xpathSApply(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:Enzymes/x:Enzyme/x:EnzymeName/x:cvParam',namespaces=ns, xmlAttrs)['name', ], 
      semiSpecific=as.logical(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:Enzymes/x:Enzyme/@semiSpecific', namespaces=ns))
    )
    parameters$ParentTolerance <- data.frame(t(xpathSApply(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ParentTolerance/x:cvParam', namespaces=ns, fun=xmlAttrs))[,c('value', 'unitName', 'name')], stringsAsFactors=FALSE)
    parameters$ParentTolerance <- data.frame(lapply(as.list(parameters$ParentTolerance), type.convert, as.is=TRUE), stringsAsFactors=FALSE)
    parameters$ModificationParams <- data.frame(t(xpathSApply(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification', namespaces=ns, fun=xmlAttrs)), stringsAsFactors=FALSE)
    parameters$ModificationParams <- data.frame(lapply(as.list(parameters$ModificationParams), type.convert, as.is=TRUE), stringsAsFactors=FALSE)
    parameters$ModificationParams$name <- unlist(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification/x:cvParam/@name', namespaces=ns))
    new(Class='mzIDparameters', software=software, rawFile=rawFile, databaseFile=databaseFile, idFile=idFile, parameters=parameters)
  }
}