library(XML)
f1 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_tandem.mzid"
f2 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/55merge_omssa.mzid"
f3 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Sequest_example_ver1.1.mzid"
f4 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Mascot_NA_example.mzid"
f5 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Mascot_top_down_example.mzid"
f6 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/MPC_example_Multiple_search_engines.mzid"
f7 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/mascot_pmf_example.mzid"
f8 <- "http://psi-pi.googlecode.com/svn/trunk/examples/1_1examples/Sequest_example_ver1.1.mzid"
doc1 <- xmlInternalTreeParse(f1)
doc2 <- xmlInternalTreeParse(f2)
doc3 <- xmlInternalTreeParse(f3)
doc4 <- xmlInternalTreeParse(f4)
doc5 <- xmlInternalTreeParse(f5)
doc6 <- xmlInternalTreeParse(f6)
doc7 <- xmlInternalTreeParse(f7)
doc8 <- xmlInternalTreeParse(f8)