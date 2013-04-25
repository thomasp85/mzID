#' @include generics.R
#' @include aaa.R
NULL

#' A class to store peptide information from an mzIdentML file
#' 
#' This class handles parsing and storage of peptide information from mzIDentML files, residing at the
#' /x:MzIdentML/x:SequenceCollection/x:Peptide node.
#' 
#' The information is stored in a dataframe with an id, an optinal name and the amino acid sequence of the peptide.
#' Alongside a list is stored with modification information of each peptide. Each row in the dataframe has a
#' corresponding entry en the list. If no modification of the peptide is present the entry is NULL, if a modification
#' is present the entry is a dataframe, listing the different modifications of the peptide.
#' 
#' @name mzIDpeptides-class
#' 
#' @section Objects from the class:
#' Objects of mzIDpeptides are not meant to be created explicitly but as part of the \code{\link{mzID-class}}. Still
#' object can be created with the constructor \code{\link{mzIDpeptides}} (not exported).
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{peptides}:}{A data.frame containing all peptides used in the search}
#'  \item{\code{modifications}:}{A list containing possible modifications of the peptides listed in @@peptides}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{length}:}{Reports the number of peptides}
#' }
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDpeptides}}
#' 
#' @rdname mzIDpeptides-class
#'
setClass(
		'mzIDpeptides',
		representation=representation(
				peptides = 'data.frame',
				modifications = 'list'
		),
		validity=function(object){
			if(nrow(object@peptides) != length(object@modifications) & length(object@modifications) != 0){
				stop('Dimensions must match between elements')
			}
		},
		prototype=prototype(
				peptides = data.frame(),
				modifications = list()
		)
)

#' Show method for mzIDpeptides objects
#' 
#' This function reports general information on the mzIDpeptides object. It is called automatically when an object is querried.
#' 
#' @param object An mzIDpeptides object
#' 
#' @return A description of the content of the mzIDpeptides object
#' 
#' @seealso \code{\link{mzIDpeptides-class}}
#' 
setMethod(
		'show', 'mzIDpeptides',
		function(object){
			if(length(object) == 0){
				cat('An empty mzIDpeptides object\n')
			} else {
				cat('An mzIDpeptides object containing: ', length(unique(object@peptides$id)), ' peptides (', sum(object@peptides$modified), ' modified)\n', sep='')
			}
		}
)

#' Report the length of an mzIDpeptides object
#' 
#' The length of an mzIDpeptides object is defined as the number of peptides in the @@peptides slot. An empty object has a length of 0
#' 
#' @param x An mzIDpeptides object
#' 
#' @return A \code{numeric} giving the number of peptides in the mzIDpeptides object
#' 
#' @seealso \code{\link{mzIDpeptides-class}}
#' 
setMethod(
		'length', 'mzIDpeptides',
		function(x){
			nrow(x@peptides)
		}
)

#' A constructor for the mzIDpeptides class
#' 
#' This function handles parsing of data and construction of an mzIDpeptides object. This function is not intended to be called
#' explicitly but as part of an mzID construction. Thus, the function is not exported.
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns The appropriate namespace for the doc, as a named character vector with the namespace named x
#' 
#' @return An \code{mzIDpeptides} object
#' 
#' @seealso \code{\link{mzIDpeptides-class}}
#' 
mzIDpeptides <- function(doc, ns){
  if(missing(doc)){
    new(Class='mzIDpeptides')
  } else {
    pepID <- attrExtract(doc, ns, path="/x:MzIdentML/x:SequenceCollection/x:Peptide")
    pepSeq <- xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide", namespaces=ns, fun=xmlValue)
    modDF <- attrExtract(doc, ns, path="/x:MzIdentML/x:SequenceCollection/x:Peptide/x:Modification")
    if(nrow(modDF) > 0){
      modName <- xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide/x:Modification/x:cvParam", namespaces=ns, fun=xmlAttrs)['name', ]
      nModPepID <- countChildren(doc, ns, path="/x:MzIdentML/x:SequenceCollection/x:Peptide", 'Modification')
      pepDF <- data.frame(pepID, pepSeq, modified=nModPepID > 0, stringsAsFactors=FALSE)
      modList <- list()
      modList[nModPepID > 0] <- split(modDF, rep(1:length(nModPepID), nModPepID))
    } else {
      pepDF <- data.frame(pepID, pepSeq, modified=FALSE, stringsAsFactors=FALSE)
      modList <- list()
    }
    new(Class='mzIDpeptides',peptides=pepDF, modifications=modList)
  }
}
