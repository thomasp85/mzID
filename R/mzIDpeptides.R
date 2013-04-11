#' @include generics.R
NULL

setClass(
		'mzIDpeptides',
		representation=representation(
				peptides = 'data.frame',
				modifications = 'list'
		),
		validity=function(object){
			if(nrow(object@peptides) != length(object@modifications)){
				stop('Dimensions must match between elements')
			}
		},
		prototype=prototype(
				peptides = data.frame(),
				modifications = list()
		)
)
setMethod(
		'show', 'mzIDpeptides',
		function(object){
			if(length(object) == 0){
				cat('An empty mzIDpeptides object\n')
			} else {
				cat('An mzIDpeptides object containing: ', length(unique(object@peptides$pepID)), ' peptides (', sum(object@peptides$modified), ' modified)\n', sep='')
			}
		}
)
setMethod(
		'length', 'mzIDpeptides',
		function(x){
			length(x@modifications)
		}
)
mzIDpeptides <- function(doc, ns){
  if(missing(doc)){
    new(Class='mzIDpeptides')
  } else {
    pepID <- xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide", namespaces=ns, fun=xmlAttrs)
    pepSeq <- xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide", namespaces=ns, fun=xmlValue)
    mod <- t(xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide/x:Modification", namespaces=ns, fun=xmlAttrs))
    modName <- xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide/x:Modification/x:cvParam", namespaces=ns, fun=xmlAttrs)['name', ]
    nModPepID <- xpathSApply(doc, path="/x:MzIdentML/x:SequenceCollection/x:Peptide", namespaces=ns, fun=xmlSize)-1
    pepDF <- data.frame(pepID, pepSeq, modified=nModPepID>0)
    modDF <- data.frame(massDelta=as.numeric(mod[,'monoisotopicMassDelta']), location=as.numeric(mod[,'location']), name=modName)
    modList <- list()
    modList[nModPepID > 0] <- split(modDF, rep(1:length(nModPepID), nModPepID))
    new(Class='mzIDpeptides',peptides=pepDF, modifications=modList)
  }
}
