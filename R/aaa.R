#' Report the number of children with specific name per node in an XML document
#' 
#' This function takes an xpath argument and reports the number of children with given names at each
#' node. Several children can be specified
#' 
#' Given an xmlInternalDocument, a namespace, an xpath argument and a number of names this function will
#' count, for each node in the xpath argument, how many children with the specified name(s) exists. It is
#' optimized for several child names, so that the performance hit with additional names are neglectible.
#' 
#' An additional constraint on the match can be given with the \code{withPar} argument, which requests
#' that each child, in addition to having the correct name, also should contain an attribute with the
#' name given in the \code{withPar} argument, in order to be counted. This search requires addition look-
#' ups in the XML document and will thus give a performance hit.
#' 
#' If our XML document had the following structure, and \code{doc} and \code{ns} was set up correctly
#' 
#' \code{<node/>}\cr
#' \code{__<child1 value='5'/>}\cr
#' \code{__<child1 value='1'/>}\cr
#' \code{__<child2/>}\cr
#' \code{</node>}\cr
#' \code{<node/>}\cr
#' \code{__<child1/>}\cr
#' \code{__<child2/>}\cr
#' \code{__<child2/>}\cr
#' \code{</node>}\cr\cr
#' 
#' \code{countChildren(doc, ns, path='./x:node', child='child1')}
#' 
#' would return:\cr
#' \code{[1] 2 1}
#' 
#' \code{countChildren(doc, ns, path='./x:node', child='child1', withPar='value')}
#' 
#' would return:\cr
#' \code{[1] 2 0}
#' 
#' \code{countChildren(doc, ns, path='./x:node', child='child1', simplify=FALSE)}
#' 
#' would return:\cr
#' \code{$child1}\cr
#' \code{[1] 2 1}
#' 
#' \code{countChildren(doc, ns, path='./x:node', child=c('child1', 'child2')}
#' 
#' would return:\cr
#' \code{$child1}\cr
#' \code{[1] 2 1}\cr\cr
#' \code{$child2}\cr
#' \code{[1] 1 2}\cr\cr
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns A string with name \code{x} givin the namespace for the \code{doc}
#' 
#' @param path An xpath expression giving the nodes to search in
#' 
#' @param child A character vector giving the name of the children to search for
#' 
#' @param withPar (optional) A character string giving a name of an attribute that must exist in the child
#' for it to be considred in counting.
#' 
#' @param simplify (default=TRUE) A logical value giving whether the output should be returned as a
#' dataframe if only one child is given
#' 
#' @param addFinalizer \code{Logical} Sets whether reference counting should be turned on
#' 
#' @return If \code{length(child) == 1} and \code{simplify == TRUE} a vector giving the number of matches
#' per node. If \code{length(child) > 1} or \code{simplify == FALSE} a named list with an element per
#' child argument containing a vector giving the number of matches per node.
#' 
#' @importFrom XML xpathApply xmlAttrs xmlChildren
#' 
countChildren <- function(doc, ns, path, child, withPar, simplify=TRUE, addFinalizer=FALSE){
    children <- xpathApply(doc, path=path, namespaces=ns, fun=xmlChildren, addFinalizer=addFinalizer)
    if(length(children) == 0){
        warning('The specified XPATH expression is empty')
        ansErr <- rep(0, length(child))
        names(ansErr) <- child
        if(simplify && length(child) == 1){
            ansErr <- ansErr[[1]]
        } else {}
        ansErr
    } else {
        ans <- list()
        for(i in 1:length(child)){
            childrenMatch <- names(unlist(children)) == child[i]
            if(!missing(withPar)){
                attrs <- lapply(unlist(children), xmlAttrs)[childrenMatch]
                names(attrs) <- NULL
#                attrs <- xpathApply(doc, path=paste(path, '/x:', child, sep=''), namespaces=ns, xmlAttrs)[childrenMatch]
                attrs[sapply(attrs, is.null)] <- NA
                nodeLength <- sapply(attrs, length)
                hasName <- sapply(split(names(unlist(attrs)) == withPar, rep(seq(along=nodeLength), nodeLength)), any)
                childrenMatch[childrenMatch] <- hasName
            } else {}
            nChildren <- sapply(children, length)
            nChildren[nChildren != 0] <- sapply(split(childrenMatch, rep(seq(along=nChildren), nChildren)), sum)
            ans[[i]] <- nChildren
        }
        names(ans) <- child
        if(simplify && length(child) == 1){
            ans <- ans[[1]]
        } else {}
        ans
    }
}

#' An extension of type.convert() to handle XML like logicals (lower case)
#' 
#' This function is a simple extension to the \code{\link[utils]{type.convert}} function. It handles the
#' extra case where a vector of lower case true and false value would not be converted to logicals. As
#' this is the standard format in XML files, this extension is needed
#' 
#' @param ... Parameters passed on to \code{type.convert}
#' 
#' @seealso \code{\link[utils]{type.convert}}
#' 
type.convert <- function(...){
    x <- utils::type.convert(...)
    if(all(unique(x) %in% c('true', 'false'))){
        x <- as.logical(x)
    } else {}
    x
}

#' Extract and properly format node attributes
#' 
#' This is a function to mainly handle correct formatting of the output from \code{\link[XML]{xmlAttrs}}.
#' In addition it allows for multiple named child nodes to be specified.
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns A string with name \code{x} givin the namespace for the \code{doc}
#' 
#' @param path An xpath expression giving the nodes to search in
#' 
#' @param child A character vector giving the name of the children to search for
#' 
#' @param addFinalizer \code{Logical} Sets whether reference counting should be turned on
#' 
#' @return If \code{length(child) == 1} a data.frame with a column for each attribute name and rows giving
#' the value in each node. If \code{length(child) > 1} a named list containing a data.frame for each child.
#' NA values are inserted if an attribute is missing from a node.
#' 
#' @importFrom XML xpathSApply xmlAttrs
#' @importFrom plyr rbind.fill.matrix
#' 
attrExtract <- function(doc, ns, path, child, addFinalizer=FALSE){
    if(missing(child)){
        attr <- xpathApply(doc, path=path, namespaces=ns, fun=xmlAttrs, addFinalizer=addFinalizer)
        if(is.list(attr)){
            isNULL <- sapply(attr, is.null)
            attr <- attr[!isNULL]
            attr <- rbind.fill.matrix(lapply(attr, function(x) if(!is.null(x)) t(x) else matrix()))
            colnames <- colnames(attr)
            attr <- replace(matrix(NA, length(isNULL), ncol(attr)), !isNULL, attr)
            colnames(attr) <- colnames
        } else if(length(attr) == 0){
            attr <- matrix()
        } else if(is.matrix(attr)){
            attr <- t(attr)
        } else {
            attrName <- unique(names(attr))
            attr <- data.frame(attr, stringsAsFactors=FALSE)
            names(attr) <- attrName
        }
        attr <- data.frame(attr, stringsAsFactors=FALSE)
        attr <- data.frame(lapply(attr, type.convert, as.is=TRUE), stringsAsFactors=FALSE)
        attr
    } else {
        attr <- list()
        for(i in 1:length(child)){
            pathChild <- paste(path, '/x:', child[i], sep='')
            attr[[i]] <- attrExtract(doc, ns, pathChild, addFinalizer=addFinalizer)
        }
        names(attr) <- child
        attr
    }
}

#' Extract and format attributes from nodes with name and value attributes
#' 
#' This function handles the special case of extracting information from nodes with the attributes name
#' and value. This function treats the value of the name attribute as the category for the value of the
#' value attribute, so that it in essence equals an XML node of the form \code{<node category='value'/>}.
#' Several child names can be specified and the result will in the end be merged to a single data.frame
#' rows for each node in the path argument.
#' 
#' @param doc an \code{XMLInternalDocument} created using \code{\link[XML]{xmlInternalTreeParse}}
#' 
#' @param ns A string with name \code{x} givin the namespace for the \code{doc}
#' 
#' @param path An xpath expression giving the nodes to search in
#' 
#' @param child A character vector giving the name of the children to search for
#' 
#' @param addFinalizer \code{Logical} Sets whether reference counting should be turned on
#' 
#' @return A data.frame with columns for each unique value in the name attribute of the nodes search in.
#' The data.frame will have rows for each node in the path expression, regardless of whether it contains
#' children.
#' 
attrExtractNameValuePair <- function(doc, ns, path, child, addFinalizer=FALSE){
    lengthOut <- getNodeSet(doc, namespaces=ns, path=paste('count(', path, ')', sep=''), addFinalizer=addFinalizer)
    attr <- attrExtract(doc, ns, path, child, addFinalizer=addFinalizer)
    nAttrChild <- countChildren(doc, ns, path, child, simplify=FALSE, addFinalizer=addFinalizer)
    ans <- list()
    for(i in 1:length(attr)){
        if(all(c('value', 'name') %in% names(attr[[i]]))){
            attrSplit <- split(attr[[i]]$value, attr[[i]]$name)
            attrIndex <- split(rep(1:length(nAttrChild[[i]]), nAttrChild[[i]]), attr[[i]]$name)
            attrFinal <- data.frame(matrix(NA, ncol=length(attrSplit), nrow=lengthOut))
            names(attrFinal) <- names(attrSplit)
            for(j in 1:length(attrSplit)){
                attrFinal[attrIndex[[j]], j] <- attrSplit[[j]]
            }
            ans[[i]] <- attrFinal
        } else {}
    }
    ans <- ans[!sapply(ans, is.null)]
    do.call('cbind', ans)
}


#' Get the mzIdentML version and check that the version is supported
#' 
#' Currently version 1.1 and 1.0 are supported.
#' 
#' @param ns The namespace of the mzIdentML file
#' 
#' @return A textstring giving the version of the mzIdentML file if it is supported. In case of missing support 
#' it throws an error.
#' 
getVersion <- function(ns) {
    v <- strsplit(ns, "/")[[1]]
    v <- v[length(v)]
    if (!v %in% c("1.0", "1.1"))
        stop("Version ", v, " unknown: only 1.0 and 1.1 supported.") 
    return(v)
}


#' Get the correct namespace path depending on version
#' 
#' This function resolves an inconsistancy between version 1.0 and 1.1 where the namespace is changed in case.
#' 
#' @param ns The namespace of the mzIdentML file
#' 
#' @return A textstring with the top parent node of the DOM
#' 
getPath <- function(ns) {
    v <- getVersion(ns)
    if (v == "1.0") {
        path <- '/x:mzIdentML'
    } else if(v == "1.1"){
        path <- '/x:MzIdentML'
    } else {
        stop("Version ", v, " unknown: only 1.0 and 1.1 supported.")
    }
    return(path)
}

colNamesToLower <- function(x) {
    colnames(x) <- casefold(colnames(x), upper=FALSE)
    x
}

#' Parses an xml file and defines the namespace
#' 
#' @param path The path to the xml file
#' 
#' @param addFinalizer \code{Logical} Sets whether reference counting should be turned on
#' 
#' @return A list with the named elements: doc: the results of xmlInternalTreeParse and ns: the namespace as a vector.
#' 
#' @importFrom XML xmlInternalTreeParse
prepareXML <- function(path, addFinalizer=FALSE) {
    doc <- xmlInternalTreeParse(path, addFinalizer=addFinalizer)
    namespaceDef <- getDefaultNamespace(doc)
    ns <- c(x=namespaceDef[[1]]$uri)
    list(doc=doc, ns=ns)
}

#' Test whether a given string is a remote resource
#' 
#' @param x the path to the xml file
#' 
#' @return logical
#' 
isRemote <- function(x) {
  grepl("^(http|ftp)[s]?://", x)
}

