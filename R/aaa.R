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
#' \code{<node/>\cr
#' __<child1 value='5'/>\cr
#' __<child1 value='1'/>\cr
#' __<child2/>\cr
#' </node>\cr
#' <node/>\cr
#' __<child1/>\cr
#' __<child2/>\cr
#' __<child2/>\cr
#' </node>\cr\cr}
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
#' @return If \code{length(child) == 1} and \code{simplify == TRUE} a vector giving the number of matches
#' per node. If \code{length(child) > 1} or \code{simplify == FALSE} a named list with an element per
#' child argument containing a vector giving the number of matches per node.
#' 
countChildren <- function(doc, ns, path, child, withPar, simplify=TRUE){
  children <- xpathApply(doc, path=path, namespaces=ns, fun=xmlChildren)
  ans <- list()
  for(i in 1:length(child)){
    childrenMatch <- names(unlist(children)) == child[i]
    if(!missing(withPar)){
      attrs <- xpathApply(doc, path=paste(path, '/x:', child, sep=''), namespaces=ns, xmlAttrs)
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

#' An extension of type.convert() to handle XML like logicals (lower case)
#' 
#' This function is a simple extension to the \code{\link[utils]{type.convert}} function. It handles the
#' extra case where a vector of lower case true and false value would not be converted to logicals. As
#' this is the standard format in XML files, this extension is needed
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
#' @return If \code{length(child) == 1} a data.frame with a column for each attribute name and rows giving
#' the value in each node. If \code{length(child) > 1} a named list containing a data.frame for each child.
#' NA values are inserted if an attribute is missing from a node.
#' 
attrExtract <- function(doc, ns, path, child){
  if(missing(child)){
    attr <- xpathSApply(doc, path=path, namespaces=ns, fun=xmlAttrs)
    if(is.list(attr)){
      attr <- rbind.fill.matrix(lapply(attr, t))
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
      attr[[i]] <- attrExtract(doc, ns, pathChild)
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
#' @return A data.frame with columns for each unique value in the name attribute of the nodes search in.
#' The data.frame will have rows for each node in the path expression, regardless of whether it contains
#' children.
#' 
attrExtractNameValuePair <- function(doc, ns, path, child){
  lengthOut <- getNodeSet(doc, namespaces=ns, path=paste('count(', path, ')', sep=''))
  attr <- attrExtract(doc, ns, path, child)
  nAttrChild <- countChildren(doc, ns, path, child)
  ans <- list()
  for(i in 1:length(child)){
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
