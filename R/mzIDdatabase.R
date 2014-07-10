#' @include generics.R
#' @include aaa.R
NULL

#' A class to store database information from an mzIdentML file
#' 
#' This class handles parsing and storage of database information from mzIDentML files, residing at the
#' /MzIdentML/SequenceCollection/DBSequence node.
#' 
#' The content of the class is stored in a data.frame with columns depending on the content of the mzIdentML
#' file. Required information for files conforming to the mzIdentML standard are: 'accession', 'searchDatabase_ref'
#' and 'id', while additional information can fx be 'length' (number of residues), 'description' (from the fasta file)
#' and 'sequence' (the actual sequence).
#' 
#' @name mzIDdatabase-class
#' 
#' @section Objects from the class:
#' Objects of mzIDdatabase are not meant to be created explicitly but as part of the \code{\link{mzID-class}}. Still
#' object can be created with the constructor \code{\link{mzIDdatabase}} (not exported).
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{database}:}{A data.frame containing references to all the database sequences from the mzIdentML file}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{length}:}{Reports the number of entries in the database}
#' }
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDdatabase}}
#' 
#' @rdname mzIDdatabase-class
#' 
setClass(
    'mzIDdatabase',
    slots=list(
        database='data.frame'
    ),
    prototype=prototype(
        database=data.frame()
    )
)

#' Show method for mzIDdatabase objects
#' 
#' This function reports general information on the mzIDdatabase object. It is called automatically when an object is querried.
#' 
#' @param object An mzIDdatabse object
#' 
#' @return A description of the content of the mzIDdatabase object
#' 
#' @seealso \code{\link{mzIDdatabase-class}}
#' 
#' @noRd
#' 
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

#' Report the length of an mzIDdatabase object
#' 
#' The length of an mzIDdatabase object is defined as the number of entries in the databse. An empty object has a length of 0
#' 
#' @param x An mzIDdatabase object
#' 
#' @return A \code{numeric} giving the number of entries in the mzIDdatabase object
#' 
#' @seealso \code{\link{mzIDdatabase-class}}
#' 
#' @noRd
#' 
setMethod(
    'length', 'mzIDdatabase',
    function(x){
        nrow(x@database)
    }
)
#' See mzID-getters
#' 
#' @noRd
#' 
setMethod(
    'database', 'mzIDdatabase',
    function(object, safeNames=TRUE){
        if(safeNames) {
            colNamesToLower(object@database)
        } else {
            object@database
        }
    }
)
#' A constructor for the mzIDdatabase class
#' 
#' This function handles parsing of data and construction of an mzIDdatabase object. This function is not intended to be called
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
#' @return An \code{mzIDdatabase} object
#' 
#' @seealso \code{\link{mzIDdatabase-class}}
#' 
#' @importFrom XML getNodeSet xmlValue
#' @export
#' 
mzIDdatabase <- function(doc, ns, addFinalizer=FALSE, path){
    if (missing(doc)) {
        if (missing(path)) {
            return(new(Class = 'mzIDdatabase'))
        } else {
            xml <- prepareXML(path)
            doc <- xml$doc
            ns <- xml$ns
        }
    }
    .path <- getPath(ns)
    database <- attrExtract(doc, ns, paste0(.path, '/x:SequenceCollection/x:DBSequence'), addFinalizer=addFinalizer)
    dbnames <- getNodeSet(doc,
                          paste0(.path, '/x:SequenceCollection/x:DBSequence/x:cvParam/@name'),
                          namespaces = ns,
                          addFinalizer=addFinalizer)
    if(length(dbnames) > 0){
        dbnames1 <- unlist(getNodeSet(doc,
                                      paste0(.path, '/x:SequenceCollection/x:DBSequence/x:cvParam/@value'),
                                      namespaces=ns,
                                      addFinalizer=addFinalizer))[dbnames == 'protein description']
        hasName <- countChildren(doc, ns,
                                 path=paste0(.path, '/x:SequenceCollection/x:DBSequence'),
                                 'cvParam', 'name', addFinalizer=addFinalizer)
        hasRightName <- as.logical(hasName)
        hasRightName[hasRightName] <-
            sapply(split(dbnames == 'protein description', rep(seq(along=hasName), hasName)), any)
        dbnames1 <- mapply(sub, paste('^\\Q',database$accession[hasRightName], '\\E', ' ', sep=''), '', dbnames1)
        database$description <- NA
        database$description[hasRightName] <- dbnames1
    }
    dbseq <- getNodeSet(doc, paste0(.path, '/x:SequenceCollection/x:DBSequence/x:Seq'), namespaces = ns, addFinalizer=addFinalizer)
    if (length(dbseq) > 0) {
        dbseq <- sapply(dbseq, xmlValue)
        hasSeq <- countChildren(doc, ns, path=paste0(.path, '/x:SequenceCollection/x:DBSequence'), 'Seq', addFinalizer=addFinalizer)
        database$sequence <- NA
        database$sequence[hasSeq != 0] <- dbseq
    }
    new(Class = 'mzIDdatabase',
        database = database)
}
