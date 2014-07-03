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
#'  \item{\code{\link{flatten}}:}{...}
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
setMethod(
    'length', 'mzID',
    function(x){
        length(x@psm)
    })

#' @rdname flatten-methods
#' 
#' @param no.redundancy \code{Logical} Should duplicate peptides be removed. Default is \code{FALSE} as identical peptides from different proteins should normally be kept.
#' 
setMethod(
    'flatten', 'mzID',
    function(object, no.redundancy=FALSE) {
        flatPSM <- flatten(object@psm)
        flatPSM <- flatPSM[, colnames(flatPSM) != 'id']
        flatEviData <- 
            cbind(object@evidence@evidence,
                  object@database@database[
                      match(object@evidence@evidence$dbsequence_ref,
                            object@database@database$id), ])
        flatEviData <- flatEviData[,!names(flatEviData) == 'id']
        flatPep <- flatten(object@peptides)
        flatPepEviData <- 
            merge( flatPep, flatEviData, 
                   by.x="id", by.y="peptide_ref", all=TRUE)
        if (no.redundancy) {
            flatPepEviData <- 
                flatPepEviData[!duplicated(flatPepEviData[,'id']),]
        }
        flatAll <- merge(flatPSM, flatPepEviData, 
                         by.x='peptide_ref', by.y='id', all=TRUE)
        flatAll$spectrumFile <- 
            object@parameters@rawFile$name[
                match(flatAll$spectradata_ref,
                      object@parameters@rawFile$id)]
        flatAll$databaseFile <- 
            object@parameters@databaseFile$name[
                match(flatAll$searchdatabase_ref,
                      object@parameters@databaseFile$id)]
        flatAll <- flatAll[, !grepl('_ref$', 
                                    names(flatAll), 
                                    perl=T) & 
                               !names(flatAll) == 'id']
        return(flatAll)
    })