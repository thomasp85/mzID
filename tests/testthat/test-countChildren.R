testfile <- system.file('extdata', 'testfile.xml', package='mzID')
doc <- xmlInternalTreeParse(testfile)
namespaceDef <- getDefaultNamespace(doc)
ns <- c(x=namespaceDef[[1]]$uri)

context('countChildren')

test_that('countChildren works when one child is specified', {
    expect_that(countChildren(doc, ns, path='/x:MzIdentML/x:TestSpace/x:countChildren/x:node', child='child1'), equals(c(2,1)))
    expect_that(countChildren(doc, ns, path='/x:MzIdentML/x:TestSpace/x:countChildren/x:node', child='dummy'), equals(c(0,0)))
    expect_that(suppressWarnings(countChildren(doc, ns, path='/x:MzIdentML/x:TestSpace/x:countChildren/x:dummyNode', child='child1')), equals(0))
    expect_that(countChildren(doc, ns, path='/x:MzIdentML/x:TestSpace/x:countChildren/x:dummyNode', child='child1'), gives_warning('The specified XPATH expression is empty'))
})

test_that('countChildren works when several children are specified', {
    expect_that(countChildren(doc, ns, path='/x:MzIdentML/x:TestSpace/x:countChildren/x:node', child=c('child1', 'child2')), equals(list(child1=c(2,1), child2=c(1,2))))
})

test_that('countChildren correctly filter the counts based on the withPar parameter', {
    expect_that(countChildren(doc, ns, path='/x:MzIdentML/x:TestSpace/x:countChildren/x:node', child='child1', withPar='value'), equals(c(2,0)))
})