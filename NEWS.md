# BGmisc 1.3.6
* revived checkParents function to check for handling phantom parents and missing parents
* added tests for checkParents function
* added GoT analysis
* reduced complexity of com2links, summarizePedigree, and checkIDs with the use of subfunctions
* allow verbose argument to be passed to standardizeColnames
* list SimPed and related_coef as aliases for functions
* harmonizing function names like calcFamilySize from famSizeCal
* implemented adjBeta function to evaluation alternative build method
* reorganize file names to be more consistent

# BGmisc 1.3.5.1
* Setting the default for the `sparse` argument in `ped2com()` to TRUE

# BGmisc 1.3.5
* Added ASOIAF pedigree
* Added com2links() function to convert components to kinship links, with accompanying tests
* Added extractWikiFamilyTree() function to parse family trees from wiki templates, with accompanying tests
* Created vignette demonstrating adjacency matrix methods and applications
* Improved plotPedigree() function by silencing unnecessary invisible list outputs
* Added checkPedigreeNetwork() function for validating pedigree network structure, with accompanying tests

# BGmisc 1.3.4.1
* Hot fix to resolve issue with list of adjacency matrix not loading saved version
* Reoptimized generation calculation

# BGmisc 1.3.4
* Added alternative (and faster) methods to create the adjacency matrix
* Add tests for comparison of adjacency matrix build methods
* Added Royal Family pedigree

# BGmisc 1.3.3
* Added ability to save and reload pedigree objects that are used by ped2Com
* Optimized generation calculation
* Added more tests for summarizePedigree
* enhanced documentation

# BGmisc 1.3.2.1
* Added ability to pass additional arguments to the ped2FOO functions

# BGmisc 1.3.2
* Added some more tests of identifyModel.R
* Modified tests to be MKL friendly

# BGmisc 1.3.1
* Confirmed that all orcids are correct
* Added gedcom importer

# BGmisc 1.3.0.1
* Created subfunctions to reduce function complexity

# BGmisc 1.3.0
* Harmonized function names
* Fixed incorrectly spelled last name in Potter pedigree
* Added function to summarize variables by family, matrilinael, and patrilineal lines
* Added within row duplicate ID checks
* Added data validation vignettes
* Harmonized function names and arguments

# BGmisc 1.2.1

* Added alternative transpose options for the matrix
* Added generalization of Falconer's formula

# BGmisc 1.2.0

* Added numerous code checks, increased code coverage to 85%
* Replaced sapply usage
* Added additional data validation checks
* Accompanying paper published in the Journal of Open Source Software

# BGmisc 1.1.0

* Added ability to simulate twins
* Can now trace paternal and maternal lines
* There's now a Harry Potter pedigree

# BGmisc 1.0.1

* Hot fix to resolve plotPedigree wrapper function breaking for pedigrees that contained multiple families

# BGmisc 1.0

* Added major update to include simulations, plotting, and examples.

# BGmisc 0.1

* Added a `NEWS.md` file to track changes to the package.
* Initial version launched
