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
#' The mzID class stores information in a subset of classes, each class having its own slot. While
#' These classes should not need to be accessed directly, but descriptions of their content is
#' delegated to each respective class.
#' 
#' @name mzID-class
#' 
#' @section Objects from the class:
#' Objects can be created using the \code{\link{mzID}} constructor, which handles parsing of mzIdentML files
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{parameters}:}{An instance of \code{\link{mzIDparameters-class}}. This object contains all information related to how the analysis was carried out.}
#'  \item{\code{psm}:}{An instance of \code{\link{mzIDpsm-class}}. This object contains the meat of the analysis with all scans and their related PSMs recorded.}
#'  \item{\code{peptides}:}{An instance of \code{\link{mzIDpeptides-class}}. This object contains a library of all peptides generated from the database along with possible modifications.}
#'  \item{\code{evidence}:}{An instance of \code{\link{mzIDevidence-class}}. This object lists all peptides detected in the analysis with reference to the \code{mzIDpeptides} instance.}
#'  \item{\code{database}:}{An instance of \code{\link{mzIDdatabase-class}}. This object contains information on the proteins in the database. As the full database is not recorded in mzIdentML files the actual protein sequence is not recorded but there is sufficient information to retrieve it from the database file.}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{\link{length}}:}{Reports the number of PSMs in the object.}
#'  \item{\code{\link{flatten}}:}{Flattens the mzID object to a tabular format}
#'  \item{\code{\link{removeDecoy}}:}{Remove decoy related information from the mzID object, potentionally trimming the size down substantially}
#'  \item{\code{\link{database}}:}{Returns a data frame with information pertaining to the database used during peptide search}
#'  \item{\code{\link{evidence}}:}{Returns a data frame with information pertaining to the evidences (sequences from proteins) found during peptide search}
#'  \item{\code{\link{peptides}}:}{Returns a data frame with information pertaining to the peptides detected during a peptide search. Peptides can link to several evidence if the same sequence is reoccuring in multiple proteins}
#'  \item{\code{\link{modifications}}:}{Returns a list with information on the modification state of all peptides}
#'  \item{\code{\link{id}}:}{Returns a data frame with information pertaining to identification of specific peptides}
#'  \item{\code{\link{scans}}:}{Returns a data frame with references to the scans in the raw file that have returned a match}
#'  \item{\code{\link{idScanMap}}:}{Returns a list linking scans with identification result}
#' }
#' 
#' @seealso \code{\link{mzID}} \code{\link{mzIDparameters-class}} \code{\link{mzIDpsm-class}} \code{\link{mzIDpeptides-class}} \code{\link{mzIDevidence-class}} \code{\link{mzIDdatabase-class}}
#' 
#' @references \url{http://www.psidev.info/mzidentml}
#' 
#' @exportClass mzID
#' @rdname mzID-class
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

#' Show method for mzID objects
#' 
#' This function reports general information on the mzID object. It is called automatically when an object is querried.
#' 
#' @param object An mzID object
#' 
#' @return A description of the content of the mzID object
#' 
#' @seealso \code{\link{mzID-class}}
#' 
#' @noRd
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

#' Report the length of an mzID object
#' 
#' The length of an mzID object is defined as the number of PSMs. An empty object has a length of 0
#' 
#' @param x An mzID object
#' 
#' @return A \code{numeric} giving the number of PSMs in the mzID object
#' 
#' @seealso \code{\link{mzID-class}}
#' 
#' @noRd
#' 
setMethod(
    'length', 'mzID',
    function(x){
        length(x@psm)
    })

#' see flatten
#' 
#' @noRd
#' 
setMethod(
    'flatten', 'mzID',
    function(object, safeNames=TRUE) {
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
        flatAll$spectrumFile <- 
            object@parameters@rawFile$name[
                match(flatAll$spectradata_ref,
                      object@parameters@rawFile$id)]
        flatAll$databaseFile <- 
            object@parameters@databaseFile$name[
                match(flatAll$searchdatabase_ref,
                      object@parameters@databaseFile$id)]
        flatAll <- flatAll[, !grepl('_ref$', 
                                    tolower(names(flatAll)), 
                                    perl=T) & 
                               !tolower(names(flatAll)) == 'id']
        return(flatAll)
    }
)

#' See removeDecoy
#' 
#' @noRd
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

#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'database', 'mzID',
    function(object, safeNames=TRUE){
        database(object@database, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'evidence', 'mzID',
    function(object, safeNames=TRUE){
        evidence(object@evidence, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'parameters', 'mzID',
    function(object){
        parameters(object@parameters)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'software', 'mzID',
    function(object){
        software(object@parameters)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'files', 'mzID',
    function(object){
        files(object@parameters)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'peptides', 'mzID',
    function(object, safeNames=TRUE){
        peptides(object@peptides, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'modifications', 'mzID',
    function(object){
        modifications(object@peptides)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'id', 'mzID',
    function(object, safeNames=TRUE){
        id(object@psm, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'scans', 'mzID',
    function(object, safeNames=TRUE){
        scans(object@psm, safeNames=safeNames)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'idScanMap', 'mzID',
    function(object){
        idScanMap(object@psm)
    }
)
