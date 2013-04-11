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
#' @section Slots
#' 
#'
#' @exportClass mzID-class
#' @rdname mzID-class
#' 
setClass(
	'mzID',
	representation=representation(
		parameters='mzIDparameters',
    psm='mzIDpsm',
    peptides='mzIDpeptides',
    evidence='mzIDevidence',
    database='mzIDdatabase'
    ),
  prototype=prototype(
    parameters=mzIDparameters(),
    psm=mzIDpsm(),
    peptides=mzIDpeptides(),
    evidence=mzIDevidence(),
    database=mzIDdatabase()
    )
  )
setMethod(
  'show', 'mzID',
  function(object){
    if(length(object) == 0){
      cat('An empty mzID object\n')
    } else {
      cat('An mzID object\n\n')
      cat('Software used:\t', object@parameters@software$name[1], ' (version: ', object@parameters@software$version[1], ')\n', sep='')
      if(nrow(object@parameters@software) > 1){
        for(i in 2:nrow(object@parameters@software)){
          cat('\t\t\t\t', object@parameters@software$name[i], ' (version ', object@parameters@software$version[i], ')\n', sep='')
        }
      } else {}
      if(nrow(object@parameters@rawFile) == 1){
        cat('Rawfile:\t\t', object@parameters@rawFile$location[1], '\n', sep='')        
      } else {
        cat('Rawfiles:\t\t', object@parameters@rawFile$location[1], '\n', sep='')
        for(i in 2:nrow(object@parameters@rawFile)){
          cat('\t\t\t\t', object@parameters@rawFile$location[i], '\n', sep='')
        }
      }
      cat('Database:\t\t', object@parameters@databaseFile, '\n\n', sep='')
      cat('Number of scans:', nrow(object@psm@scans), '\n')
      cat('Number of PSM\'s:', nrow(object@psm@id), '\n')
    }
  })
setMethod(
  'length', 'mzID',
  function(x){
    length(x@parameters)
  }
  )

#' Parse an mzIdentML file
#' 
#' This function takes a single mzIdentML file and parses it into an mzID object.
#' 
#' The mzID function uses the XML package to read the content of an mzIdentML file and store it in
#' an mzID object. Unlike how mzR handles mzML files, mzID parses everything in one chunk. Memory
#' can thus be a problem for very big datasets, but as mzIdentML files are not indexed, it is
#' ineficient to access the data dynamically.
#' 
#' @param file A character string giving the location of the mzIdentML file to be parsed
#' 
#' @return An mzID object
#' 
#' @export
mzID <- function(file){
  if(missing(file)){
    new(Class='mzID')
  } else {
    doc = xmlInternalTreeParse(file)
    ns=c(x="http://psidev.info/psi/pi/mzIdentML/1.1")
    new(Class='mzID',
        parameters=mzIDparameters(file, doc, ns),
        psm=mzIDpsm(doc, ns),
        peptides=mzIDpeptides(doc, ns),
        evidence=mzIDevidence(doc, ns),
        database=mzIDdatabase(doc, ns)
        )
  }
}