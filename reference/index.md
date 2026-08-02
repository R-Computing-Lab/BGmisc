# Package index

## GEDCOM I/O

- [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
  [`readGed()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
  [`readgedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
  : Read a GEDCOM File
- [`gedcomLatToNumeric()`](https://r-computing-lab.github.io/BGmisc/reference/gedcomLatToNumeric.md)
  : Convert GEDCOM Latitude String to Numeric
- [`gedcomLonToNumeric()`](https://r-computing-lab.github.io/BGmisc/reference/gedcomLonToNumeric.md)
  : Convert GEDCOM Longitude String to Numeric

## Other functions

- [`addMaternalChain()`](https://r-computing-lab.github.io/BGmisc/reference/addMaternalChain.md)
  : Add maternal ancestor chains to a pedigree

- [`addMaternalLineFlag()`](https://r-computing-lab.github.io/BGmisc/reference/addMaternalLineFlag.md)
  : Add a maternal-line descendant flag to a pedigree

- [`addParentRow()`](https://r-computing-lab.github.io/BGmisc/reference/addParentRow.md)
  : Create a properly formatted parent row for the pedigree

- [`addParentalChain()`](https://r-computing-lab.github.io/BGmisc/reference/addParentalChain.md)
  : Add unilineal parental ancestor chains to a pedigree

- [`addParentalLineFlag()`](https://r-computing-lab.github.io/BGmisc/reference/addParentalLineFlag.md)
  : Add a unilineal parental-line descendant flag to a pedigree

- [`addPaternalChain()`](https://r-computing-lab.github.io/BGmisc/reference/addPaternalChain.md)
  : This is a convenience wrapper around \[addParentalChain()\] with
  \`component = "dadID"\`.

- [`addPaternalLineFlag()`](https://r-computing-lab.github.io/BGmisc/reference/addPaternalLineFlag.md)
  : Add a paternal-line descendant flag to a pedigree

- [`addRowlessParents()`](https://r-computing-lab.github.io/BGmisc/reference/addRowlessParents.md)
  : Add addRowlessParents

- [`alignPhenToMatrix()`](https://r-computing-lab.github.io/BGmisc/reference/alignPhenToMatrix.md)
  : Align Phenotype Vector to Matrix Format for OpenMx

- [`applyTagMappings()`](https://r-computing-lab.github.io/BGmisc/reference/applyTagMappings.md)
  : Apply Tag Mappings to a Line

- [`buildBtwnGenerations()`](https://r-computing-lab.github.io/BGmisc/reference/buildBtwnGenerations.md)
  : Process Generation Connections

- [`buildFamilyGroups()`](https://r-computing-lab.github.io/BGmisc/reference/buildFamilyGroups.md)
  : Build family group models

- [`buildOneFamilyGroup()`](https://r-computing-lab.github.io/BGmisc/reference/buildOneFamilyGroup.md)
  : Build one family group model

- [`buildPedigreeModelCovariance()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeModelCovariance.md)
  : Create an mxModel for a pedigree

- [`buildPedigreeMx()`](https://r-computing-lab.github.io/BGmisc/reference/buildPedigreeMx.md)
  : Build Pedigree mxModel

- [`buildWithinGenerations()`](https://r-computing-lab.github.io/BGmisc/reference/buildWithinGenerations.md)
  : Process Generations for Pedigree Simulation

- [`calcAllGens()`](https://r-computing-lab.github.io/BGmisc/reference/calcAllGens.md)
  [`allGens()`](https://r-computing-lab.github.io/BGmisc/reference/calcAllGens.md)
  :

  calcAllGens A function to calculate the number of individuals in each
  generation. This is a supporting function for `simulatePedigree`.

- [`calcFamilySize()`](https://r-computing-lab.github.io/BGmisc/reference/calcFamilySize.md)
  [`famSizeCal()`](https://r-computing-lab.github.io/BGmisc/reference/calcFamilySize.md)
  :

  calcFamilySize A function to calculate the total number of individuals
  in a pedigree given parameters. This is a supporting function for
  function `simulatePedigree`

- [`calcFamilySizeByGen()`](https://r-computing-lab.github.io/BGmisc/reference/calcFamilySizeByGen.md)
  [`sizeAllGens()`](https://r-computing-lab.github.io/BGmisc/reference/calcFamilySizeByGen.md)
  :

  calcFamilySizeByGen An internal supporting function for
  `simulatePedigree`.

- [`calculateCIs()`](https://r-computing-lab.github.io/BGmisc/reference/calculateCIs.md)
  : Confidence Intervals for Correlations with Optional Design-Effect
  Adjustment

- [`calculateH()`](https://r-computing-lab.github.io/BGmisc/reference/calculateH.md)
  : Falconer's Formula

- [`calculateRelatedness()`](https://r-computing-lab.github.io/BGmisc/reference/calculateRelatedness.md)
  [`related_coef()`](https://r-computing-lab.github.io/BGmisc/reference/calculateRelatedness.md)
  : Calculate Relatedness Coefficient

- [`checkIDs()`](https://r-computing-lab.github.io/BGmisc/reference/checkIDs.md)
  : Validates and Optionally Repairs Unique IDs in a Pedigree Dataframe

- [`checkIDuniqueness()`](https://r-computing-lab.github.io/BGmisc/reference/checkIDuniqueness.md)
  : Check for duplicated individual IDs

- [`checkParentIDs()`](https://r-computing-lab.github.io/BGmisc/reference/checkParentIDs.md)
  : Validates and Optionally Repairs Parent IDs in a Pedigree Dataframe

- [`checkParentSex()`](https://r-computing-lab.github.io/BGmisc/reference/checkParentSex.md)
  : Check Parental Role Sex Consistency

- [`checkPedigreeNetwork()`](https://r-computing-lab.github.io/BGmisc/reference/checkPedigreeNetwork.md)
  : Validate Pedigree Network Structure

- [`checkSex()`](https://r-computing-lab.github.io/BGmisc/reference/checkSex.md)
  : Validates and Optionally Repairs Sex Coding in a Pedigree Dataframe

- [`checkWithinRowDuplicates()`](https://r-computing-lab.github.io/BGmisc/reference/checkWithinRowDuplicates.md)
  : Check for within-row duplicates (self-parents, same mom/dad)

- [`collapseNames()`](https://r-computing-lab.github.io/BGmisc/reference/collapseNames.md)
  : collapse Names

- [`com2links()`](https://r-computing-lab.github.io/BGmisc/reference/com2links.md)
  : Convert Sparse Relationship Matrices to Kinship Links

- [`comp2vech()`](https://r-computing-lab.github.io/BGmisc/reference/comp2vech.md)
  : comp2vech Turn a variance component relatedness matrix into its
  half-vectorization

- [`computeParentAdjacency()`](https://r-computing-lab.github.io/BGmisc/reference/computeParentAdjacency.md)
  : Compute Parent Adjacency Matrix with Multiple Approaches

- [`condenseMatrixSlots()`](https://r-computing-lab.github.io/BGmisc/reference/condenseMatrixSlots.md)
  : Condense Matrix Slots in an OpenMx Model

- [`countPatternRows()`](https://r-computing-lab.github.io/BGmisc/reference/countPatternRows.md)
  : Count GEDCOM Pattern Rows

- [`createGenDataFrame()`](https://r-computing-lab.github.io/BGmisc/reference/createGenDataFrame.md)
  [`createGenDataFrame_beta()`](https://r-computing-lab.github.io/BGmisc/reference/createGenDataFrame.md)
  : Create Data Frame for Generation

- [`determineSex()`](https://r-computing-lab.github.io/BGmisc/reference/determineSex.md)
  [`determineSex_beta()`](https://r-computing-lab.github.io/BGmisc/reference/determineSex.md)
  : Determine Sex of Offspring

- [`.addPersonToPed()`](https://r-computing-lab.github.io/BGmisc/reference/dot-addPersonToPed.md)
  : addPersonToPed

- [`.adjBeta()`](https://r-computing-lab.github.io/BGmisc/reference/dot-adjBeta.md)
  : Construct Adjacency Matrix for Parent-Child Relationships Using Beta
  Methods This function constructs an adjacency matrix for parent-child
  relationships using a method in beta testing. It identifies
  parent-child pairs based on the specified component of relatedness.

- [`.adjDirect()`](https://r-computing-lab.github.io/BGmisc/reference/dot-adjDirect.md)
  : Construct Adjacency Matrix for Parent-Child Relationships Using
  Direct Method

- [`.adjIndexed()`](https://r-computing-lab.github.io/BGmisc/reference/dot-adjIndexed.md)
  : Construct Adjacency Matrix for Parent-Child Relationships Using
  Indexed Method

- [`.assignParentValue()`](https://r-computing-lab.github.io/BGmisc/reference/dot-assignParentValue.md)
  : Assign parent values based on component type

- [`.computeTranspose()`](https://r-computing-lab.github.io/BGmisc/reference/dot-computeTranspose.md)
  : Compute the transpose multiplication for the relatedness matrix

- [`.loadOrComputeIsChild()`](https://r-computing-lab.github.io/BGmisc/reference/dot-loadOrComputeIsChild.md)
  : Load or compute the isChild matrix

- [`.postProcessGedcom.legacy()`](https://r-computing-lab.github.io/BGmisc/reference/dot-postProcessGedcom.legacy.md)
  : Post-process GEDCOM Data Frame

- [`dropIdenticalDuplicateIDs()`](https://r-computing-lab.github.io/BGmisc/reference/dropIdenticalDuplicateIDs.md)
  : Drop Identical Duplicate IDs from Pedigree Data Frame

- [`dropLink()`](https://r-computing-lab.github.io/BGmisc/reference/dropLink.md)
  : dropLink

- [`findBiggest()`](https://r-computing-lab.github.io/BGmisc/reference/findBiggest.md)
  :

  Function to find the biggest families in a pedigree This function
  finds the biggest families in a pedigree. It is supposed to be used
  internally by the `summarize_pedigree` function.

- [`findLeaves()`](https://r-computing-lab.github.io/BGmisc/reference/findLeaves.md)
  : Find Leaf Nodes in a Pedigree

- [`findOldest()`](https://r-computing-lab.github.io/BGmisc/reference/findOldest.md)
  :

  Function to find the oldest individuals in a pedigree This function
  finds the oldest families in a pedigree. It is supposed to be used
  internally by the `summarize_pedigree` function.

- [`fitComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitComponentModel.md)
  : fitComponentModel Fit the estimated variance components of a model
  to covariance data

- [`fitPedigreeModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitPedigreeModel.md)
  : Fit an OpenMx pedigree model to observed data

- [`fuseTwins()`](https://r-computing-lab.github.io/BGmisc/reference/fuseTwins.md)
  : Fuse MZ twin pairs in a pedigree dataset for path tracing This
  function identifies MZ twin pairs in the pedigree dataset and merges
  their IDs for path tracing purposes. The second twin in each pair is
  made a founder (with NA parents), and all children of the second twin
  are redirected to the first twin. This allows for correct relatedness
  calculations without diagonal or downstream artifacts.

- [`getGenDist()`](https://r-computing-lab.github.io/BGmisc/reference/getGenDist.md)
  : Compute the generational distance between two individuals

- [`hazard`](https://r-computing-lab.github.io/BGmisc/reference/hazard.md)
  : Simulated pedigree with two extended families and an age-related
  hazard

- [`identifyComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/identifyComponentModel.md)
  : identifyComponentModel Determine if a variance components model is
  identified

- [`inbreeding`](https://r-computing-lab.github.io/BGmisc/reference/inbreeding.md)
  : Artificial pedigree data on eight families with inbreeding

- [`initializeRecord()`](https://r-computing-lab.github.io/BGmisc/reference/initializeRecord.md)
  : Initialize an Empty Individual Record

- [`insertEven()`](https://r-computing-lab.github.io/BGmisc/reference/insertEven.md)
  [`evenInsert()`](https://r-computing-lab.github.io/BGmisc/reference/insertEven.md)
  : evenInsert A function to insert m elements evenly into a length n
  vector.

- [`makeInbreeding()`](https://r-computing-lab.github.io/BGmisc/reference/makeInbreeding.md)
  : makeInbreeding

- [`makeTwins()`](https://r-computing-lab.github.io/BGmisc/reference/makeTwins.md)
  : makeTwins

- [`mapFAMS2parents()`](https://r-computing-lab.github.io/BGmisc/reference/mapFAMS2parents.md)
  : Create a Mapping from Family IDs to Parent IDs

- [`markPotentialChildren()`](https://r-computing-lab.github.io/BGmisc/reference/markPotentialChildren.md)
  [`markPotentialChildren_beta()`](https://r-computing-lab.github.io/BGmisc/reference/markPotentialChildren.md)
  : Mark and Assign children

- [`parseNameLine()`](https://r-computing-lab.github.io/BGmisc/reference/parseNameLine.md)
  : Parse Name Line

- [`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md)
  : Take a pedigree and turn it into an additive genetics relatedness
  matrix

- [`ped2addFocal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2addFocal.md)
  : Add a focal-person additive relatedness column to a pedigree

- [`ped2ce()`](https://r-computing-lab.github.io/BGmisc/reference/ped2ce.md)
  : Take a pedigree and turn it into an extended environmental
  relatedness matrix

- [`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md)
  : Take a pedigree and turn it into a common nuclear environmental
  matrix

- [`ped2cnFocal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cnFocal.md)
  : Add a focal-person common nuclear relatedness column to a pedigree

- [`ped2com()`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)
  : Take a pedigree and turn it into a relatedness matrix

- [`ped2fam()`](https://r-computing-lab.github.io/BGmisc/reference/ped2fam.md)
  : Segment Pedigree into Extended Families

- [`ped2focal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2focal.md)
  : Compute relatedness between all individuals and a focal person

- [`ped2gen()`](https://r-computing-lab.github.io/BGmisc/reference/ped2gen.md)
  : Take a pedigree and turn it into a generation relatedness matrix. It
  computes the rank of the generation matrix, which is the number of
  generations separating two individuals

- [`ped2genDist()`](https://r-computing-lab.github.io/BGmisc/reference/ped2genDist.md)
  : Compute a full pairwise generational distance matrix

- [`ped2genDistFocal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2genDistFocal.md)
  : Compute generational distances from a focal individual to all others

- [`ped2genFocal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2genFocal.md)
  : Add a focal-person generation relatedness column to a pedigree. It
  computes the rank of the generation matrix, which is the number of
  generations separating two individuals

- [`ped2graph()`](https://r-computing-lab.github.io/BGmisc/reference/ped2graph.md)
  : Turn a pedigree into a graph

- [`ped2maternal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2maternal.md)
  : Add a maternal line ID variable to a pedigree

- [`ped2mit()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md)
  : Take a pedigree and turn it into a mitochondrial relatedness matrix

- [`ped2mitFocal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mitFocal.md)
  : Add a focal-person mitochondrial relatedness column to a pedigree

- [`ped2paternal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2paternal.md)
  : Add a paternal line ID variable to a pedigree

- [`postProcessGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/postProcessGedcom.md)
  : Post-process GEDCOM Data Frame

- [`potter`](https://r-computing-lab.github.io/BGmisc/reference/potter.md)
  : Fictional pedigree data on a wizarding family

- [`prepSummarizePedigrees()`](https://r-computing-lab.github.io/BGmisc/reference/prepSummarizePedigrees.md)
  : Function to prepare the pedigree for summarization This function
  prepares the pedigree for summarization by ensuring that the necessary
  IDs are present and that the pedigree is built correctly.

- [`processEventLine()`](https://r-computing-lab.github.io/BGmisc/reference/processEventLine.md)
  : Process Event Lines (Birth or Death)

- [`processParents()`](https://r-computing-lab.github.io/BGmisc/reference/processParents.md)
  : Process Parents Information from GEDCOM Data

- [`readGedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
  [`readGed()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
  [`readgedcom()`](https://r-computing-lab.github.io/BGmisc/reference/readGedcom.md)
  : Read a GEDCOM File

- [`readWikifamilytree()`](https://r-computing-lab.github.io/BGmisc/reference/readWikifamilytree.md)
  : Read Wiki Family Tree

- [`recodeSex()`](https://r-computing-lab.github.io/BGmisc/reference/recodeSex.md)
  : Recodes Sex Variable in a Pedigree Dataframe

- [`repairIDs()`](https://r-computing-lab.github.io/BGmisc/reference/repairIDs.md)
  : Repair Missing IDs

- [`repairParentIDs()`](https://r-computing-lab.github.io/BGmisc/reference/repairParentIDs.md)
  : Repair Parent IDs

- [`repairSex()`](https://r-computing-lab.github.io/BGmisc/reference/repairSex.md)
  : Repairs Sex Coding in a Pedigree Dataframe

- [`restorePedColnames()`](https://r-computing-lab.github.io/BGmisc/reference/restorePedColnames.md)
  : Restore Original Column Names in a Pedigree Dataframe

- [`royal92`](https://r-computing-lab.github.io/BGmisc/reference/royal92.md)
  : Royal pedigree data from 1992

- [`simulatePedigree()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
  [`SimPed()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
  : Simulate Pedigrees This function simulates "balanced" pedigrees
  based on a group of parameters: 1) k - Kids per couple; 2) G - Number
  of generations; 3) p - Proportion of males in offspring; 4) r - Mating
  rate.

- [`simulatePedigrees()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigrees.md)
  : Simulate Multiple Pedigrees

- [`sliceFamilies()`](https://r-computing-lab.github.io/BGmisc/reference/sliceFamilies.md)
  : sliceFamilies

- [`summarizeFamilies()`](https://r-computing-lab.github.io/BGmisc/reference/summarizeFamilies.md)
  [`summariseFamilies()`](https://r-computing-lab.github.io/BGmisc/reference/summarizeFamilies.md)
  : Summarize the families in a pedigree

- [`summarizeMatrilines()`](https://r-computing-lab.github.io/BGmisc/reference/summarizeMatrilines.md)
  [`summariseMatrilines()`](https://r-computing-lab.github.io/BGmisc/reference/summarizeMatrilines.md)
  : Summarize the maternal lines in a pedigree

- [`summarizePatrilines()`](https://r-computing-lab.github.io/BGmisc/reference/summarizePatrilines.md)
  [`summarisePatrilines()`](https://r-computing-lab.github.io/BGmisc/reference/summarizePatrilines.md)
  : Summarize the paternal lines in a pedigree

- [`summarizePedigrees()`](https://r-computing-lab.github.io/BGmisc/reference/summarizePedigrees.md)
  [`summarisePedigrees()`](https://r-computing-lab.github.io/BGmisc/reference/summarizePedigrees.md)
  : Summarize Pedigree Data

- [`traceTreePaths()`](https://r-computing-lab.github.io/BGmisc/reference/traceTreePaths.md)
  : Trace paths between individuals in a family tree grid

- [`trimPedigree()`](https://r-computing-lab.github.io/BGmisc/reference/trimPedigree.md)
  : Iteratively Trim Leaf Nodes from a Pedigree

- [`validate_and_convert_matrix()`](https://r-computing-lab.github.io/BGmisc/reference/validate_and_convert_matrix.md)
  : validate_and_convert_matrix

- [`vech()`](https://r-computing-lab.github.io/BGmisc/reference/vech.md)
  : vech Create the half-vectorization of a matrix
