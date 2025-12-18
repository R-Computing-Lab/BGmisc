# BGmisc beta 1.5.2
* More flexible ID generation for simulatePedigree
* Created ped2gen function to extract generation information from pedigree data.frames
* Added tests for ped2gen
* Fixed handling of character ID variables leading to a warning in ped2fam

# BGmisc 1.5.1
## CRAN submission
* partially refactored summarizePedigree to be more modular
* added compression control to ped2com
* Minor copy editing


# BGmisc 1.5.0
## CRAN submission
* Removed ASOIAF dataset from BGmisc, now in ggpedigree
* Enhancing potter family tree
* updated tests to handle the transition of ASOIAF data to ggpedigree
* smarter aliases
* smarter CIs
* tweaking variable inputs

# BGmisc 1.4.4
* Updated ASOIAF pedigree to include Tarths, Brackens
* Added sliceFamilies function with working tests
* Better messaging for sliceFamilies
* Now has some error handling for sliceFamilies
* Added option to restore variable names in selected places

# BGmisc 1.4.3.2
* Slightly expanded vignettes to include more examples of the new features in BGmisc and ggpedigree.
* Updated ASOIAF pedigree to reduce missing parents.
* Added tests to check if data are acyclic
* reduce file size for royal92

# BGmisc 1.4.3.1

* Updated ASOIAF pedigree to reduce missing parents.

# BGmisc 1.4.3
* transferred plotPedigree to ggpedigree

# BGmisc 1.4.2
* Added twinIDs for potter and asoiaf pedigrees
* Added twinID to simulatePedigree function, and extended to include MZ, DZ, and SS twins.
* Added a few more tests for simulatePedigree
* Added function to easily add new person to a pedigree
* Updated ASOIAF pedigree to reduce missing parents
* Added a few more tests for simulatePedigree helpers
* Allow simulatePedigree to accept variable names and coding for sex

# BGmisc 1.4.1
* replaced print with message in all functions
* Exposed several internal functions to the user
* refactored addPhantomParents to be more efficient
* add mtdna and mitochondria as aliases for mitochondrial
* reorganized unit tests
* introduces the new calculateCIs function for computing confidence intervals for correlation coefficients
* added comprehensive tests and corresponding documentation updates for calculateCI

# BGmisc 1.4.0
* revived checkParents function to check for handling phantom parents and missing parents
* added tests for checkParents function
* added GoT analysis
* reduced complexity of com2links, summarizePedigree, and checkIDs with the use of subfunctions
* allow verbose argument to be passed to standardizeColnames
* list SimPed and related_coef as aliases for functions
* harmonizing function names like calcFamilySize from famSizeCal
* implemented adjBeta function to evaluation alternative build method
* reorganize file names to be more consistent
* harmonized famID

# BGmisc 1.3.5.1
* Setting the default for the `sparse` argument in `ped2com()` to TRUE

# BGmisc 1.3.5
* Add calculateCIs and readDelimitedData functions
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
