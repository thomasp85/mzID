# mzID

mzID is a package that allows parsing of mzIdentML files. The mzIdentML file format has been created by [HUPO](http://www.psidev.info/mzidentml) as a standardized format for collecting data from peptide identification analyses based on LC-MS type experiments. The parser is kept bare bone in order to make it as general as possible.

mzID is part of [Bioconductor](http://www.bioconductor.org) and can be installed through there.

### Main Features

* `mzID(filename)` parses the mzIdentML file at the location specified by filename into an mzID object
* `flatten(mzID_object)` flatten the content of the mzID object into a data frame with a row for each psm
* The parser is written to support the multitude of different ways peptide identification can be performed and thus be recorded in the mzIdentML format
* Defines a class to handle a collection of identification results efficiently
* Relies heavily on the `XML` package (which uses libxml2) for parsing
* Respects the fact that peptide identification results are not inherently tabular in format. The mzID class contains several different classes each handling different part of the analysis data
* The parser is being optimized for speed within the bounds of needing to be general for all mzIdentML files. This means that a parser only handling a very specific analysis pipeline could likely be faster, but still the parser is fairly fast

### Roadmap

* Make the parser software aware, so that it can create more beautiful output based on the software used in the analysis
* Performance improvements
