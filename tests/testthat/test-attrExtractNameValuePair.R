testfile <- system.file('extdata', 'testfile.xml', package='mzID')
doc <- xmlInternalTreeParse(testfile)
namespaceDef <- getDefaultNamespace(doc)
ns <- c(x=namespaceDef[[1]]$uri)

context('attrExtractNameValuePair')

test_that('attrExtractNameValuePair handles missing values', {
    expect_that(attrExtractNameValuePair(doc, ns, path='/x:MzIdentML/x:TestSpace/x:attrExtractNameValuePair/x:node', child='child1'), equals(data.frame(a=c(1,2), b=c(3, NA))))
    expect_that(attrExtractNameValuePair(doc, ns, path='/x:MzIdentML/x:TestSpace/x:attrExtractNameValuePair/x:node', child=c('child1', 'child2')), equals(data.frame(a=c(1,2), b=c(3, NA), c=c(10, NA))))
})