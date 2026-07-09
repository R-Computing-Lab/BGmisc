# BGmisc NEWS

## Development version

* Added temporal pedigree models. 
* Added make clean personids for error handling and debugging.

## BGmisc 1.9.0
* Added `addParentalChain()` and `addParentalFlag()` for tracing parental lineages. These functions support general parental-chain construction and convenient maternal and paternal lineage workflows. `addParentalFlag()` adds a binary indicator for whether each individual belongs to a specified parental chain, which can be useful for filtering, grouping, and lineage-specific summaries.
* Fixed silent mis-scoring in `ped2com()`, `ped2add()`, and related component wrappers when momID or dadID referenced a parent ID that was not present as its own row in ped, such as an unrecorded founder or a parent excluded from a pedigree subset. Previously, isChild_method = "partialparent" treated these parents as known because their IDs were non-missing, while adjacency builders treated the corresponding parent-child link as absent. This could understate diagonal values and remove covariance between siblings who shared the missing rowless parent.
* `ped2com()` now warns when rowless parents are detected. The new repair_rowless_parents argument provides two repair strategies:
    * "rows" adds one placeholder founder row for each unique missing parent ID to a working copy of ped, then restricts the returned matrix back to the original individuals through keep_ids, unless keep_ids is already supplied.
    * "schur" applies a Schur-complement update to the block-triangular RAM system. For each missing parent, its known children define a rank-1 update, v %*% t(v), where v is that parent's traced genetic contribution to every individual. This update is added to the relatedness matrix at the tcrossprod step. The "schur" method currently supports only component = "additive".

## BGmisc 1.8.0
* Optimized the GEDCOM reader and com2links() for speed and memory usage, especially for large pedigrees.
* Fixed a GEDCOM reader bug that caused document records to be added to the final person in the pedigree.
* Added unit tests for the GEDCOM reader and data parser.
* Improved GEDCOM parsing, including more robust event parsing, better support for different GEDCOM versions, and improved usability.
* Optimized `sliceFamilies()` to be more general and to no longer require mitochondrial DNA information.
* Added `.require_openmx()` to make OpenMx-dependent functionality easier to use without making OpenMx a package dependency.
* Improved string ID handling in `ped2id()`.
* Fixed handling of different-sized matrices in `com2links()`.
* Added `alignPhenToMatrix()` to align phenotypic data to the order of a relatedness matrix.
* Added `simulatePedigrees()` to simulate multiple families at once and return them as a single combined data frame.
* Refactored OpenMx wrapper functions.
* Added `ped2focal()` and component-specific wrappers, including `ped2addFocal()`, `ped2mitFocal()`, `ped2mtFocal()`, `ped2cnFocal()`, and `ped2genFocal()`, to compute relatedness between all pedigree members and a focal individual. These functions append the focal relatedness values to the pedigree data frame, with individuals excluded by keep_ids coded as NA and genuine zero values retained.
* Added `getGenDist()`, `ped2genDistFocal()`, and `ped2genDist()` for computing generational distance between individuals. Supported methods include generation-rank differences, shortest parent-child paths through shared ancestors, and most-recent-common-ancestor based distances. tput fs includeongle pairs, a focal column appended to the pedigree, and a full n×n pairwise matrixix.
* Optimized `countPatternRows()` in the GEDCOM reader by using fixed string matching and a pre-extracted column vector, reducing redundant work across repeated pattern checks.

# BGmisc 1.7.0.0
* Fixed a bug in parList.
* Moved ped2com() wrappers to their own .R file.
* Fixed a missing checkpoint for ram_checkpoint.
* Added a chunk_size argument to `ped2com()` to reduce memory usage during matrix transposition.
* Added an individual-ID filtering method for selecting whose relatedness values are returned.
* Renamed `ytemp` parameter to `obs_ids` in `buildOneFamilyGroup()` and `buildFamilyGroups()` for clarity
* Expanded v6 vignettes with a data requirements reference and a real-data workflow using the hazard dataset.
* Added support for confidence intervals in pedigree OpenMx wrappers.

# BGmisc 1.6.0.1
* CRAN submission
* Add OpenMx pedigree model builders and docs
* Added vignette for OpenMx pedigree model builders
* Add option for MZ twins in the additive genetic matrix
* Add option to select sex for MZ twin generation.
* Add option to tweak pedigree with one id provided
* Add helper functions for checkParents etc
* fixed incorrect direction so that parents are pointing to children in the graphs
* Optimize simulatePedigree and helpers for speed and memory usage
* Major gains (>x10) in speed for deeper pedigrees
* Added more tests for simulatePedigree
* Fix error when not enough single people available

# BGmisc 1.5.2
* More flexible ID generation for simulatePedigree
* Created ped2gen function to extract generation information from pedigree data.frames
* Added tests for ped2gen
* Fixed handling of character ID variables leading to a warning in ped2fam
* Added famIDs to phantom parents
* Tweaked how sex coding is handled to allow for unknown sex

# BGmisc 1.5.1
* CRAN submission
* partially refactored summarizePedigree to be more modular
* added compression control to ped2com
* Minor copy editing


# BGmisc 1.5.0
* CRAN submission
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
* Added additional tests for simulatePedigree
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
