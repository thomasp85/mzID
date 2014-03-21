#' @include mzID.R
#' @include mzIDCollection.R
#' 
NULL

#' Parse an mzIdentML file
#' 
#' This function takes a single mzIdentML file and parses it into an mzID object.
#' 
#' The mzID function uses the XML package to read the content of an mzIdentML file and store it in
#' an mzID object. Unlike how mzR handles mzML files, mzID parses everything in one chunk. Memory
#' can thus be a problem for very big datasets, but as mzIdentML files are not indexed, it is
#' ineficient to access the data dynamically.
#' 
#' If multiple filenames are passed to the function they will be processed in parallel using foreach and doParallel.
#' The number of workers spawned is either the maximal number of available cores or the number of files to parse, 
#' whichever is smallest. The return value will in these cases be an mzIDCollection object. If some of the files
#' cannot be parsed they will not be contained in the returned object and a warning will be issued. No errors will
#' be thrown.
#' 
#' @param file A character string giving the location of the mzIdentML file to be parsed
#' 
#' @param verbose \code{Logical} Should information be printed to the console? Default is \code{TRUE}
#' 
#' @return An mzID object
#' 
#' @seealso \code{\link{mzID-class}} \code{\link{mzIDCollection-class}}
#' 
#' @examples
#' 
#' # Parsing of the example files provided by HUPO:
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_tandem.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_omssa.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Sequest_example_ver1.1.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Mascot_NA_example.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Mascot_top_down_example.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/MPC_example_Multiple_search_engines.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/mascot_pmf_example.mzid")
#' 
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Sequest_example_ver1.1.mzid")
#'
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Mascot_MSMS_example.mzid")
#'
#' mzID("http://psi-pi.googlecode.com/svn/trunk/examples/1_0examples/Mascot_MSMS_example.mzid")
#' @export
#' 
#' @importFrom XML xmlInternalTreeParse getDefaultNamespace
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom iterators icount
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' 
mzID <- function(file, verbose=TRUE) {
    addFinalizer = TRUE
    if (missing(file)) {
        new(Class='mzID')
    } else if(length(file) > 1) {
        nCores <- detectCores()
        nThreads <- ifelse(length(file) < nCores, length(file), nCores)
        cl <- makeCluster(nThreads, outfile=ifelse(verbose, '', NULL))
        registerDoParallel(cl)
        res <- foreach( i=icount(length(file)), .packages=c("mzID"), .errorhandling='remove') %dopar% {
                cat('reading ', basename(file[i]), '...\n', sep='')
                ans <- mzID(file[i], verbose=FALSE)
                cat(basename(file[i]), 'DONE!\n')
                gc()
                ans
            }
        stopCluster(cl)
        
        if(length(res) != length(file)) warning(length(file)-length(res), 'files could not be parsed')
        
        return(do.call('mzIDCollection', res))
    } else {
        if(verbose) cat('reading ', basename(file), '...', sep='')
        if(!file.exists(file)) stop('file ', sQuote(file), ' does not exist!')
        xml <- prepareXML(file)
        doc <- xml$doc
        ns <- xml$ns
        data <- new(Class = 'mzID',
                    parameters = mzIDparameters(doc, ns, addFinalizer=addFinalizer),
                    psm = mzIDpsm(doc, ns, addFinalizer=addFinalizer),
                    peptides = mzIDpeptides(doc, ns, addFinalizer=addFinalizer),
                    evidence = mzIDevidence(doc, ns, addFinalizer=addFinalizer),
                    database = mzIDdatabase(doc, ns, addFinalizer=addFinalizer))
        free(doc)
        free(xml$doc)
        rm(doc, ns, xml)
        gc()
        if(verbose) cat(' DONE!\n')
        return(data)
    }
}
