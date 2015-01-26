#' @include generics.R
#' @include mzIDdatabase.R
#' @include mzIDevidence.R
#' @include mzIDparameters.R
#' @include mzIDpeptides.R
#' @include mzIDpsm.R
NULL

#' A class to contain data from mzIdentML-files
#'
#' This class stores all parsed information from mzIdentML files
#'
#' The mzID class stores information in a subset of classes, each class having 
#' its own slot. While these classes should not need to be accessed directly, 
#' descriptions of their content is delegated to each respective class.
#' 
#' @section Objects from the class:
#' Objects can be created using the \code{\link{mzID}} constructor, which 
#' handles parsing of mzIdentML files
#' 
#' 
#' @slot parameters An instance of \code{\link{mzIDparameters-class}}. This 
#' object contains all information related to how the analysis was carried out.
#' 
#' @slot psm An instance of \code{\link{mzIDpsm-class}}. This object contains 
#' the meat of the analysis with all scans and their related PSMs recorded.
#' 
#' @slot peptides An instance of \code{\link{mzIDpeptides-class}}. This object 
#' contains a library of all peptides generated from the database along with 
#' possible modifications.
#' 
#' @slot evidence An instance of \code{\link{mzIDevidence-class}}. This object 
#' lists all peptides detected in the analysis with reference to the 
#' \code{mzIDpeptides} instance.
#' 
#' @slot database An instance of \code{\link{mzIDdatabase-class}}. This object 
#' contains information on the proteins in the database. As the full database is
#' not recorded in mzIdentML files the actual protein sequence is not recorded 
#' but there is sufficient information to retrieve it from the database file.
#' 
#' 
#' @family mzID-classes
#' @seealso \code{\link{mzID}}
#' 
#' @references \url{http://www.psidev.info/mzidentml}
#' 
setClass('mzID',
         slots = list(
             parameters = 'mzIDparameters',
             psm = 'mzIDpsm',
             peptides = 'mzIDpeptides',
             evidence = 'mzIDevidence',
             database = 'mzIDdatabase'),
         prototype = prototype(
             parameters = mzIDparameters(),
             psm = mzIDpsm(),
             peptides = mzIDpeptides(),
             evidence = mzIDevidence(),
             database = mzIDdatabase())
)

#' @describeIn mzID Short summary of object content
#' 
#' @param object An mzID object
#' 
setMethod('show', 'mzID',
          function(object) {
              if (length(object) == 0) {
                  cat('An empty mzID object\n')
              } else {
                  cat('An mzID object\n\n')
                  cat('Software used:   ', object@parameters@software$name[1],
                      ' (version: ', object@parameters@software$version[1], ')\n', sep='')
                  if (nrow(object@parameters@software) > 1) {
                      for(i in 2:nrow(object@parameters@software)) {
                          cat('                 ', object@parameters@software$name[i],
                              ' (version ', object@parameters@software$version[i], ')\n', sep='')
                      }
                  } else {}
                  cat('\n')
                  if(nrow(object@parameters@rawFile) == 1){
                      cat('Rawfile:         ', object@parameters@rawFile$location[1], '\n', sep='')        
                  } else {
                      cat('Rawfiles:        ', object@parameters@rawFile$location[1], '\n', sep='')
                      for(i in 2:nrow(object@parameters@rawFile)){
                          cat('                 ', object@parameters@rawFile$location[i], '\n', sep='')
                      }
                  }
                  cat('\n')
                  cat('Database:        ', object@parameters@databaseFile$location, '\n\n', sep='')
                  cat('Number of scans: ', nrow(object@psm@scans), '\n', sep='')
                  cat('Number of PSM\'s: ', nrow(object@psm@id), '\n', sep='')
              }
          })

#' @describeIn mzID Get number of psm' in object
#' 
#' @param x An mzID object
#' 
setMethod(
    'length', 'mzID',
    function(x){
        length(x@psm)
    })

#' @describeIn flatten Flatten an mzID object with respect to psm'
#' 
setMethod(
    'flatten', 'mzID',
    function(object, safeNames=TRUE) {
        if(length(object) == 0) return(data.frame())
        flatPSM <- flatten(object@psm, safeNames=safeNames)
        flatPSM <- flatPSM[, tolower(colnames(flatPSM)) != 'id']
        evi <- evidence(object, safeNames=safeNames)
        db <- database(object, safeNames=safeNames)
        flatEviData <- cbind(evi, db[match(safeCol(evi, 'dbsequence_ref'), safeCol(db, 'id')), ])
        flatEviData <- flatEviData[,!tolower(names(flatEviData)) == 'id']
        flatPep <- flatten(object@peptides, safeNames=safeNames)
        flatPepEviData <- cbind(flatEviData, flatPep[match(safeCol(flatEviData, 'peptide_ref'), safeCol(flatPep, 'id')), ])
#         flatPepEviData <- 
#             merge( flatPep, flatEviData, 
#                    by.x="id", by.y="peptide_ref", all=TRUE)
#         if (no.redundancy) {
#             flatPepEviData <- 
#                 flatPepEviData[!duplicated(flatPepEviData[,'id']),]
#         }

        peptideGroups <- split(1:nrow(flatPepEviData), safeCol(flatPepEviData, 'id'))
        peptideMatch <- match(safeCol(flatPSM, 'peptide_ref'), names(peptideGroups))
        peptideGroups <- peptideGroups[peptideMatch]
        groupLength <- sapply(peptideGroups, length)
        flatAll <- cbind(flatPSM[rep(1:nrow(flatPSM), times=groupLength),], flatPepEviData[unlist(peptideGroups),])
#         flatAll <- merge(flatPSM, flatPepEviData, 
#                          by.x='peptide_ref', by.y='id', all=TRUE)
        flatAll$idFile <- basename(object@parameters@idFile)
        flatAll$spectrumFile <- object@parameters@rawFile$name[match(safeCol(flatAll, 'spectradata_ref'), object@parameters@rawFile$id)]
        flatAll$databaseFile <- object@parameters@databaseFile$name[match(safeCol(flatAll, 'searchdatabase_ref'), object@parameters@databaseFile$id)]
        flatAll <- flatAll[, !grepl('_ref$', tolower(names(flatAll)), perl=T) & !tolower(names(flatAll)) == 'id']
        return(flatAll)
    }
)

#' @describeIn mzID Remove decoys from mzID object
#' 
setMethod(
    'removeDecoy', 'mzID',
    function(object) {
        #Start with evidence
        evi <- evidence(object, safeNames=FALSE)
        evi <- evi[!safeCol(evi, 'isdecoy'),]
        
        #Remove peptides no longer referenced in evidence
        pep <- peptides(object, safeNames=FALSE)
        index <- safeCol(pep, 'id') %in% safeCol(evi, 'peptide_ref')
        pep <- pep[index,]
        mod <- modifications(object)[index]
        
        #Remove proteins no longer referenced in evidence
        db <- database(object, safeNames=FALSE)
        db <- db[safeCol(db, 'id') %in% safeCol(evi, 'dbsequence_ref'),]
        
        #Trim psm's and scans
        nID <- id(object, safeNames=FALSE)
        index <- which(safeCol(nID, 'peptide_ref') %in% safeCol(evi, 'peptide_ref'))
        nPSM <- subsetWithMapping(nID, scans(object, safeNames=FALSE), idScanMap(object), index)
        
        nID <- nPSM$main
        nScan <- nPSM$sub
        nMapping <- nPSM$mapping
        
        new('mzID',
            parameters = object@parameters,
            psm = new('mzIDpsm', scans=nScan, id=nID, mapping=nMapping),
            peptides = new('mzIDpeptides', peptides=pep, modifications=mod),
            evidence = new('mzIDevidence', evidence=evi),
            database = new('mzIDdatabase', database=db))
    }
)

## GETTER FUNCTIONS
###################

#' @describeIn mzID Get the database used for searching
#' 
#' @param safeNames Should column names be lowercased to ensure compatibility
#' between v1.0 and v1.1 files?
#' 
setMethod(
    'database', 'mzID',
    function(object, safeNames=TRUE){
        database(object@database, safeNames=safeNames)
    }
)
#' @describeIn mzID Get the evidence from the peptide search
#' 
setMethod(
    'evidence', 'mzID',
    function(object, safeNames=TRUE){
        evidence(object@evidence, safeNames=safeNames)
    }
)
#' @describeIn mzID Get the parameters used for the search
#' 
setMethod(
    'parameters', 'mzID',
    function(object){
        parameters(object@parameters)
    }
)
#' @describeIn mzID Get the software used to arrive at the results
#' 
setMethod(
    'software', 'mzID',
    function(object){
        software(object@parameters)
    }
)
#' @describeIn mzID Get the data files used for the analysis
#' 
setMethod(
    'files', 'mzID',
    function(object){
        files(object@parameters)
    }
)
#' @describeIn mzID Get the peptides identified.
#' 
setMethod(
    'peptides', 'mzID',
    function(object, safeNames=TRUE){
        peptides(object@peptides, safeNames=safeNames)
    }
)
#' @describeIn mzID Get the modification on the identified peptides
#' 
setMethod(
    'modifications', 'mzID',
    function(object){
        modifications(object@peptides)
    }
)
#' @describeIn mzID Get the identification results
#' 
setMethod(
    'id', 'mzID',
    function(object, safeNames=TRUE){
        id(object@psm, safeNames=safeNames)
    }
)
#' @describeIn mzID Get the scans matched to peptides
#' 
setMethod(
    'scans', 'mzID',
    function(object, safeNames=TRUE){
        scans(object@psm, safeNames=safeNames)
    }
)
#' @describeIn mzID Get the link between scans and identifications
#' 
setMethod(
    'idScanMap', 'mzID',
    function(object){
        idScanMap(object@psm)
    }
)
