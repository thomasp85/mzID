#' @include generics.R
#' @include aaa.R
NULL

#' A Class to store analysis information from the mzIdentML file
#' 
#' This class tries to collect the multitude of different analysis information required to rerun the analysis. The intended
#' data to be stored are: The software used in the analysis of the data, the location and nature of the rawfile(s), the
#' location and nature of the database file(s), the location of the mzIDentML file itself as well as all the parameters used
#' during the analysis leading to the mzIdentML file. Information regarding how the LC-MS experiment was performed should be
#' collected from the raw data file. As the parameters used in different software solutions can vary greatly, all these
#' parameters are stored in a named list, which can thus be very different from pipeline to pipeline. It is the users
#' responsibility to check conformity between samples.
#' 
#' @name mzIDparameters-class
#' 
#' @section Objects from the class:
#' Objects of mzIDparameters are not meant to be created explicitly but as part of the \code{\link{mzID-class}}. Still
#' object can be created with the constructor \code{\link{mzIDparameters}} (not exported).
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{software}:}{A data frame with information retaining to the software used for the analysis. At least the name and an id is given, but optionally also version number and URI}
#'  \item{\code{rawFile}:}{A data frame with information about the raw data file(s) used for the analysis. The data frame will contain at least the location and spectrum ID format.}
#'  \item{\code{databaseFile}:}{ A data frame containing at least the location and file format of the database file used in the search.}
#'  \item{\code{idFile}:}{A character string containing the location of the mzIdentML file at the time of parsing.}
#'  \item{\code{parameters}:}{A list containing containing the information stored in the MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol node. SearchType and Threshold are the only required parameters given by the mzIdentML standard.}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{length}:}{Reports the number of raw files used in the analysis.}
#' }
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDparameters}}
#' 
#' @rdname mzIDdatabase-class
#' 
setClass(
  'mzIDparameters',
  representation=representation(
    software='data.frame',
    rawFile='data.frame',
    databaseFile='data.frame',
    idFile='character',
    parameters='list'
    ),
  prototype=prototype(
    software=data.frame(),
    rawFile=data.frame(),
    databaseFile=data.frame(),
    idFile=character(),
    parameters=list())
  )

#' Show method for mzIDparameters objects
#' 
#' This function reports general information on the mzIDparameters object. It is called automatically when an object is querried.
#' 
#' @param object An mzIDparameters object
#' 
#' @return A description of the content of the mzIDparameters object
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
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
      cat('\nDatabase:\t\t', object@databaseFile$location, '\n', sep='')
    }
  }
  )

#' Report the length of an mzIDparameters object
#' 
#' The length of an mzIDparameters object is defined as the number of raw datafiles used in the analysis. An empty object has a length of 0
#' 
#' @param x An mzIDparameters object
#' 
#' @return A \code{numeric} giving the number raw datafiles used in the analysis
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
setMethod(
  'length', 'mzIDparameters',
  function(x){
    nrow(x@rawFile)
  }
  )

#' A constructor for the mzIDparameters class
#' 
#' This function handles parsing of data and construction of an mzIDparameters object. This function is not intended to be called
#' explicitly but as part of an mzID construction. Thus, the function is not exported. It relies on a number of getter functions
#' to retrive the different information from around the document.
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return An \code{mzIDparameters} object
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
mzIDparameters <- function(doc, ns){
  if(missing(doc)){
    new(Class='mzIDparameters')
  } else {
    software <- getSoftware(doc, ns)
    idFile <- docName(doc)
    rawFile <- getRawFile(doc, ns)
    databaseFile <- getDatabaseFile(doc, ns)
    parameters <- list()
    parameters$searchType <- getSearchType(doc, ns)
    parameters$threshold <- getThreshold(doc, ns)
    parameters <- c(parameters, getAdditionalPar(doc, ns))
    parameters$enzymes <- getEnzymes(doc, ns)
    parameters$ParentTolerance <- getParentTolerance(doc, ns)
    parameters$FragmentTolerance <- getFragmentTolerance(doc, ns)
    parameters$ModificationRules <- getModifications(doc, ns)
    parameters$MassTable <- getMassTable(doc, ns)
    parameters$TranslationTable <- getDatabaseTranslation(doc, ns)
    parameters$DatabaseFilter <- getDatabaseFilter(doc, ns)
    new(Class='mzIDparameters', software=software, rawFile=rawFile, databaseFile=databaseFile, idFile=idFile, parameters=parameters)
  }
}

#' Retrive information on the software used in the analysis
#' 
#' This function collects the information regarding the analysis software used during creation of the data stored in the mzIDentML file
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with a row for each software used, and columns with at least name and id of the software
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getSoftware <- function(doc, ns){
  software <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisSoftwareList/x:AnalysisSoftware')
  software$name <- as.vector(unlist(getNodeSet(doc, path='/x:MzIdentML/x:AnalysisSoftwareList/x:AnalysisSoftware/x:SoftwareName/x:cvParam/@name', namespaces=ns)))
  if(is.null(software$version)){
    software$version <- as.character(NA)
  }
  software
}

#' Retrive information on the raw files used in the analysis
#' 
#' This function collects the information regarding the raw data files related to the analysis stored in the mzIdentML file
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with a row for each raw data file used in the analysis and at least a column for the location and the ID format of the file. 
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getRawFile <- function(doc, ns){
  rawFile <- attrExtract(doc, ns, '/x:MzIdentML/x:DataCollection/x:Inputs/x:SpectraData')
  rawFile$IDFormat <- as.vector(unlist(getNodeSet(doc, path='/x:MzIdentML/x:DataCollection/x:Inputs/x:SpectraData/x:SpectrumIDFormat/x:cvParam/@name', namespaces=ns)))
  rawFile
}

#' Retrive information on the database used in the analysis
#' 
#' This function collects the information regarding the database used in the analysis stored in the mzIdentML file
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with a at least the location of the database stored
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getDatabaseFile <- function(doc, ns){
  databaseFile <- data.frame(location=as.vector(unlist(getNodeSet(doc, '/x:MzIdentML/x:DataCollection/x:Inputs/x:SearchDatabase/@location', namespaces=ns))), stringsAsFactors=FALSE)
  if(length(getNodeSet(doc, '/x:MzIdentML/x:DataCollection/x:Inputs/x:SearchDatabase/x:FileFormat', namespaces=ns)) > 0){
    databaseFile$fileFormat=as.vector(unlist(getNodeSet(doc, '/x:MzIdentML/x:DataCollection/x:Inputs/x:SearchDatabase/x:FileFormat/x:cvParam/@name', namespaces=ns)))
  } else {}
  databaseFile
}

#' Gets the search type as specified in the mzIdentML file
#' 
#' This function simply extracts the information stored in /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/SearchType/cvParam/@@name
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{character} with the name of the search type (e.g. 'ms-ms search' or 'de novo search')
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getSearchType <- function(doc, ns){
  as.vector(unlist(getNodeSet(doc, path='/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:SearchType/x:cvParam/@name', namespaces=ns)))
}

#' Finds the psm threshold used in the file
#' 
#' This function simply extracts the attributes stored in /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/Threshold/*
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with columns 'name' and 'value' storing the tresholds used
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getThreshold <- function(doc, ns){
  threshold <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:Threshold/*')
  threshold <- threshold[, names(threshold) %in% c('name', 'value'), drop=FALSE]
  threshold
}

#' Extracts the additional, often software specific parameters
#' 
#' This function extracts and formats the information stored in the cvParam and userParam nodes in /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/AdditionalSearchParams
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{list} with names corresponding to the name attribute of the node, and content corresponding to the value attribute. If the node haven't got a a value attribute the content is set to TRUE
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getAdditionalPar <- function(doc, ns){
  addPar <- list()
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:AdditionalSearchParams/x:userParam', namespaces=ns)) > 0){
    userPar <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:AdditionalSearchParams/x:userParam')
    par <- split(userPar$value, 1:length(userPar$value))
    names(par) <- userPar$name
    par <- lapply(par, type.convert)
    addPar <- c(addPar, par)
  } else {}
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:AdditionalSearchParams/x:cvParam', namespaces=ns)) > 0){
    cvPar <- xpathApply(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:AdditionalSearchParams/x:cvParam', namespaces=ns, fun=xmlAttrs)
    parNames <- unlist(cvPar)[names(unlist(cvPar)) == 'name']
    cvPar <- lapply(cvPar, function(x) if(any(names(x) == 'value')) type.convert(x['value']) else TRUE)
    names(cvPar) <- parNames
    addPar <- c(addPar, cvPar)
  } else {}
  addPar
}

#' Extracts the enzymes used for digestion specified as specified in the software analysis
#' 
#' The function looks for information on the enzymes used in the protocol and if present extracts it
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with names of the enzymes as well as other settings related to the cleavage of proteins or \code{NULL} if no information is present
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getEnzymes <- function(doc, ns){
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:Enzymes', namespaces=ns)) > 0){
    enzymes <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:Enzymes/x:Enzyme/x:EnzymeName/x:cvParam')
    enzymeName <- getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:Enzymes/x:Enzyme/x:EnzymeName/@name', namespaces=ns)
    if(length(enzymeName > 0)){
      enzymes$name <- enzymeName
    } else {}
    enzymes[, names(enzymes) %in% c('cTermGain', 'minDistance', 'missedCleavages', 'nTermGain', 'name', 'semiSpecific'), drop=FALSE]
  } else {
    NULL
  }
}

#' Looks for presence of parent tolerance settings and reports these if found
#' 
#' This function checks for the existence of /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/ParentTolerance
#' and if found reports these.
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with the settings used for parent tolerance or \code{NULL} if none exists
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getParentTolerance <- function(doc, ns){
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ParentTolerance', namespaces=ns)) > 0){
    ParentTolerance <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ParentTolerance/x:cvParam')
    ParentTolerance
  } else {
    NULL
  }
}

#' Looks for presence of fragment tolerance settings and reports these if found
#' 
#' This function checks for the existence of /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/FragmentTolerance
#' and if found reports these.
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with the settings used for fragment tolerance or \code{NULL} if none exists
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getFragmentTolerance <- function(doc, ns){
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:FragmentTolerance', namespaces=ns)) > 0){
    FragmentTolerance <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:FragmentTolerance/x:cvParam')
    FragmentTolerance
  } else {
    NULL
  }
}

#' Looks for presence of amino acid modification settings and reports these if found
#' 
#' This function checks for the existence of /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/ModificationParams
#' and if found formats these for easy reading.
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with information on the modification settings in the search or \code{NULL} if none exists
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getModifications <- function(doc, ns){
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams', namespaces=ns)) > 0){
    ModificationParams <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification')
    if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification/x:cvParam/@name', namespaces=ns)) > 0){
      ModificationParams$name <- unlist(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification/x:cvParam/@name', namespaces=ns))      
    } else {}
    ModificationParams$Specificity <- 'any'
    if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification/x:SpecificityRules', namespaces=ns)) > 0){
      specificity <- unlist(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification/x:SpecificityRules/x:cvParam/@name', namespaces=ns))
      specificityCount <- countChildren(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification/x:SpecificityRules', 'cvParam')
      specificityExist <- countChildren(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:ModificationParams/x:SearchModification', 'SpecificityRules')
      ModificationParams$Specificity[as.logical(specificityExist)] <- sapply(split(specificity, rep(1:length(specificityCount), specificityCount)), function(x) paste(x, collapse=','))
    } else {}
    ModificationParams
  } else {
    NULL
  }
}

#' Search for the presence of a mass table used during the search
#' 
#' This function checks for the existence of /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/MassTable
#' and if found formats these for easy reading.
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with the masses used for each amino acid at the different ms levels \code{NULL} if none exists
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getMassTable <- function(doc, ns){
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:MassTable', namespaces=ns)) > 0){
    MassTable <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:MassTable/x:Residue')
    msLevel <- unlist(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:MassTable/@msLevel', namespaces=ns))
    tableCount <- countChildren(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:MassTable', 'Residue')
    MassTable$msLevel <- msLevel[rep(1:length(tableCount), tableCount)]
    MassTable
  } else {
    NULL
  }
}

#' Looks whether a translation table is present in the mzIdentML file and reports it if found
#' 
#' This function checks for the existence of /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/DatabaseTranslation
#' and if found formats these for easy reading.
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return A \code{data.frame} with a translation table or \code{NULL} if none exists
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getDatabaseTranslation <- function(doc, ns){
  if(length(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:DatabaseTranslation', namespaces=ns)) > 0){
    tables <- attrExtract(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:DatabaseTranslation/x:TranslationTable/x:cvParam')
    tables <- tables[, names(tables) %in% c('name', 'value')]
    tableNames <- unlist(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:DatabaseTranslation/x:TranslationTable/@name', namespaces=ns))
    tableCount <- countChildren(doc, ns, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:DatabaseTranslation/x:TranslationTable', 'cvParam')
    tables <- split(tables, rep(1:length(tableCount), tableCount))
    tables <- lapply(1:length(tables), function(x) list(tableName=as.vector(tableNames[x]), table=tables[[x]]$value[translationTable=tables[[x]]$name == 'translation table'], startCodons=tables[[x]]$value[translationTable=tables[[x]]$name == 'translation start codons'], description=tables[[x]]$value[translationTable=tables[[x]]$name == 'translation table description']))
    frames <- as.vector(unlist(getNodeSet(doc, '/x:MzIdentML/x:AnalysisProtocolCollection/x:SpectrumIdentificationProtocol/x:DatabaseTranslation/@frame', namespaces=ns)))
    if(is.null(frames)) {
      frames <- as.character(NA)
    } else {}
    tables <- list(Frames=frames, Tables=tables)
    tables
  } else {
    NULL
  }
}

#' Looks for any database filters applied during the peptide search
#' 
#' This function is still unimplemented, as no proper use of the /MzIdentML/AnalysisProtocolCollection/SpectrumIdentificationProtocol/DatabaseFilters have been found
#'  
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return \code{NULL}
#' 
#' @seealso \code{\link{mzIDparameters-class}}
#' 
getDatabaseFilter <- function(doc, ns){
  NULL
}