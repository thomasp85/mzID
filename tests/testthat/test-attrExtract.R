testfile <- system.file('extdata', 'testfile.xml', package='mzID')
doc <- xmlInternalTreeParse(testfile)
namespaceDef <- getDefaultNamespace(doc)
ns <- c(x=namespaceDef[[1]]$uri)

context('attrExtract')

test_that('attrExtract works with well formed input', {
    expect_that(attrExtract(doc, ns, path='/x:MzIdentML/x:TestSpace/x:attrExtract/x:node1/x:child1'), equals(data.frame(attr1=c("a", NA, NA), attr2=c('b', NA, 'd'), attr3=c('c', NA, 'e'), stringsAsFactors=F)))
    expect_that(attrExtract(doc, ns, path='/x:MzIdentML/x:TestSpace/x:attrExtract/x:node1', child=c('child1', 'child2')), equals(list(child1=data.frame(attr1=c("a", NA, NA), attr2=c('b', NA, 'd'), attr3=c('c', NA, 'e'), stringsAsFactors=F), child2=data.frame(attr1=c('f', 'a'), attr4=c('g', 'b'), stringsAsFactors=F))))
})

test_that('attrExtract handles nodes with no attributes', {
    expect_that(attrExtract(doc, ns, path='/x:MzIdentML/x:TestSpace/x:attrExtract/x:node1/x:child3'), equals(data.frame()))
})