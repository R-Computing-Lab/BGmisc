# Package index

## All functions

- [`addParentRow()`](https://r-computing-lab.github.io/BGmisc/reference/addParentRow.md)
  : Create a properly formatted parent row for the pedigree

- [`addPersonToPed()`](https://r-computing-lab.github.io/BGmisc/reference/addPersonToPed.md)
  : addPersonToPed

- [`addRowlessParents()`](https://r-computing-lab.github.io/BGmisc/reference/addRowlessParents.md)
  : Add addRowlessParents

- [`applyTagMappings()`](https://r-computing-lab.github.io/BGmisc/reference/applyTagMappings.md)
  : Apply Tag Mappings to a Line

- [`buildBetweenGenerations()`](https://r-computing-lab.github.io/BGmisc/reference/buildBetweenGenerations.md)
  : Process Generation Connections

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

- [`countPatternRows()`](https://r-computing-lab.github.io/BGmisc/reference/countPatternRows.md)
  : Count GEDCOM Pattern Rows

- [`createGenDataFrame()`](https://r-computing-lab.github.io/BGmisc/reference/createGenDataFrame.md)
  : Create Data Frame for Generation

- [`determineSex()`](https://r-computing-lab.github.io/BGmisc/reference/determineSex.md)
  : Determine Sex of Offspring

- [`.adjBeta()`](https://r-computing-lab.github.io/BGmisc/reference/dot-adjBeta.md)
  : Construct Adjacency Matrix for Parent-Child Relationships Using Beta
  Method This function constructs an adjacency matrix for parent-child
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

- [`dropLink()`](https://r-computing-lab.github.io/BGmisc/reference/dropLink.md)
  : dropLink

- [`findBiggest()`](https://r-computing-lab.github.io/BGmisc/reference/findBiggest.md)
  :

  Function to find the biggest families in a pedigree This function
  finds the biggest families in a pedigree. It is supposed to be used
  internally by the `summarize_pedigree` function.

- [`findOldest()`](https://r-computing-lab.github.io/BGmisc/reference/findOldest.md)
  :

  Function to find the oldest individuals in a pedigree This function
  finds the oldest families in a pedigree. It is supposed to be used
  internally by the `summarize_pedigree` function.

- [`fitComponentModel()`](https://r-computing-lab.github.io/BGmisc/reference/fitComponentModel.md)
  : fitComponentModel Fit the estimated variance components of a model
  to covariance data

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
  : Mark and Assign children

- [`parseNameLine()`](https://r-computing-lab.github.io/BGmisc/reference/parseNameLine.md)
  : Parse Name Line

- [`ped2add()`](https://r-computing-lab.github.io/BGmisc/reference/ped2add.md)
  : Take a pedigree and turn it into an additive genetics relatedness
  matrix

- [`ped2ce()`](https://r-computing-lab.github.io/BGmisc/reference/ped2ce.md)
  : Take a pedigree and turn it into an extended environmental
  relatedness matrix

- [`ped2cn()`](https://r-computing-lab.github.io/BGmisc/reference/ped2cn.md)
  : Take a pedigree and turn it into a common nuclear environmental
  matrix

- [`ped2com()`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)
  : Take a pedigree and turn it into a relatedness matrix

- [`ped2fam()`](https://r-computing-lab.github.io/BGmisc/reference/ped2fam.md)
  : Segment Pedigree into Extended Families

- [`ped2gen()`](https://r-computing-lab.github.io/BGmisc/reference/ped2gen.md)
  : Take a pedigree and turn it into a generation relatedness matrix

- [`ped2graph()`](https://r-computing-lab.github.io/BGmisc/reference/ped2graph.md)
  : Turn a pedigree into a graph

- [`ped2maternal()`](https://r-computing-lab.github.io/BGmisc/reference/ped2maternal.md)
  : Add a maternal line ID variable to a pedigree

- [`ped2mit()`](https://r-computing-lab.github.io/BGmisc/reference/ped2mit.md)
  : Take a pedigree and turn it into a mitochondrial relatedness matrix

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

- [`royal92`](https://r-computing-lab.github.io/BGmisc/reference/royal92.md)
  : Royal pedigree data from 1992

- [`simulatePedigree()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
  [`SimPed()`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
  : Simulate Pedigrees This function simulates "balanced" pedigrees
  based on a group of parameters: 1) k - Kids per couple; 2) G - Number
  of generations; 3) p - Proportion of males in offspring; 4) r - Mating
  rate.

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

- [`validate_and_convert_matrix()`](https://r-computing-lab.github.io/BGmisc/reference/validate_and_convert_matrix.md)
  : validate_and_convert_matrix

- [`vech()`](https://r-computing-lab.github.io/BGmisc/reference/vech.md)
  : vech Create the half-vectorization of a matrix
