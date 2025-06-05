##' Artificial pedigree data on eight families with inbreeding
##'
##' A dataset created purely from imagination that includes several types of inbreeding.
##' Different kinds of inbreeding occur in each extended family.
##'
##' The types of inbreeding are as follows:
##'
#' \itemize{
#'     \item Extended Family 1: Sister wives - Children with the same father and different mothers who are sisters.
#'     \item Extended Family 2: Full siblings have children.
#'     \item Extended Family 3: Half siblings have children.
#'     \item Extended Family 4: First cousins have children.
#'     \item Extended Family 5: Father has child with his daughter.
#'     \item Extended Family 6: Half sister wives - Children with the same father and different mothers who are half sisters.
#'     \item Extended Family 7: Uncle-niece and Aunt-nephew have children.
#'     \item Extended Family 8: A father-son pairs has children with a corresponding mother-daughter pair.
#' }
##'
##' Although not all of the above structures are technically inbreeding, they aim to test pedigree diagramming and path tracing algorithms.  This dataset is not intended to represent any real individuals or families.
##'
##' The variables are as follows:
##'
##' \itemize{
##'   \item \code{ID}:  Person identification variable
##'   \item \code{sex}:  Sex of the ID: 1 is female; 0 is male
##'   \item \code{dadID}:  ID of the father
##'   \item \code{momID}:  ID of the mother
##'   \item \code{FamID}:  ID of the extended family
##'   \item \code{Gen}:  Generation of the person
##'   \item \code{proband}:  Always FALSE
##' }
##'
##' @docType data
##' @keywords datasets
##' @name inbreeding
##' @usage data(inbreeding)
##' @format A data frame (and ped object) with 134 rows and 7 variables
NULL

##' Simulated pedigree with two extended families and an age-related hazard
##'
##' A dataset simulated to have an age-related hazard.
##' There are two extended families that are sampled from the same population.
##'
##' The variables are as follows:
##'
##' \itemize{
##'   \item \code{FamID}:  ID of the extended family
##'   \item \code{ID}:  Person identification variable
##'   \item \code{sex}:  Sex of the ID: 1 is female; 0 is male
##'   \item \code{dadID}:  ID of the father
##'   \item \code{momID}:  ID of the mother
##'   \item \code{affected}:  logical.  Whether the person is affected or not
##'   \item \code{DA1}:  Binary variable signifying the meaninglessness of life
##'   \item \code{DA2}:  Binary variable signifying the fundamental unknowability of existence
##'   \item \code{birthYr}:  Birth year for person
##'   \item \code{onsetYr}:  Year of onset for person
##'   \item \code{deathYr}:  Death year for person
##'   \item \code{available}:  logical.  Whether
##'   \item \code{Gen}:  Generation of the person
##'   \item \code{proband}:  logical.  Whether the person is a proband or not
##' }
##'
##' @docType data
##' @keywords datasets
##' @name hazard
##' @usage data(hazard)
##' @format A data frame with 43 rows and 14 variables
NULL

##' Fictional pedigree data on a wizarding family
##'
##' A dataset created  for educational and illustrative use, containing a fictional pedigree modeled after characters from the Harry Potter series.
##' This data is structured for use in software demonstrations involving pedigree diagrams, inheritance structures, and kinship modeling.
##' This dataset is not intended to represent any real individuals or families.
##' It includes no narrative content or protected expression from the original works and is provided solely for educational purposes.
##' This dataset is not endorsed by or affiliated with the creators or copyright holders of the Harry Potter series.
##'
##' The variables are as follows:
##'
##' \itemize{
##'   \item \code{personID}:  Person identification variable
##'   \item \code{famID}: Family identification variable
##'   \item \code{name}:  Name of the person
##'   \item \code{gen}: Generation of the person
##'   \item \code{momID}:  ID of the mother
##'   \item \code{dadID}:  ID of the father
##'   \item \code{spouseID}: ID of the spouse
##'   \item \code{sex}:  Sex of the ID: 1 is male; 0 is female
##'   \item \code{twinID}:  ID of the twin, if applicable
##'   \item \code{zygosity}: Zygosity of the twin, if applicable. mz is monozygotic; dz is dizygotic
##'
##' }
##'
##' IDs in the 100s \code{momID}s and \code{dadID}s are for people not in the dataset.
##'
##' @docType data
##' @keywords datasets
##' @name potter
##' @usage data(potter)
##' @format A data frame (and ped object) with 36 rows and 8 variables
NULL


##' Royal pedigree data from 1992
##'
##' A dataset created by Denis Reid from the Royal Families of Europe.
##'
##' The variables are as follows:
##' id,momID,dadID,name,sex,birth_date,death_date,attribute_title
##' \itemize{
##'   \item \code{id}:  Person identification variable
##'   \item \code{momID}:  ID of the mother
##'   \item \code{dadID}:  ID of the father
##'   \item \code{name}:  Name of the person
##'   \item \code{sex}: Biological sex
##'   \item \code{birth_date}:  Date of birth
##'   \item \code{death_date}:  Date of death
##'   \item \code{attribute_title}:  Title of the person
##'
##' }
##'
##'
##' @docType data
##' @keywords datasets
##' @name royal92
##' @usage data(royal92)
##' @format A data frame with 3110 observations
NULL

##' A pedigree of ice and fire
##'
##' A structured dataset of fictional characters derived from the Song of Ice and Fire universe by George R. R. Martin.
##' The character relationships were partially based on a GEDCOM file publicly posted in the [Westeros.org forum](https://asoiaf.westeros.org/index.php?/topic/88863-all-the-family-trees/), and were updated based on publicly available summaries from [A Wiki of Ice and Fire](https://awoiaf.westeros.org/index.php/Main_Page).
##' This dataset was created for educational and illustrative purposes, such as demonstrating pedigree construction, relationship tracing, and algorithmic logic in family-based data.
##' It includes no narrative content or protected expression from the original works.
##' No rights to the characters, names, or intellectual property of George R. R. Martin or HBO are claimed, and the dataset is not intended to represent any real individuals or families.
##'
##'
##'
##' The variables are as follows:
##' \itemize{
##'   \item \code{id}:  Person identification variable
##'   \item \code{momID}:  ID of the mother
##'   \item \code{dadID}:  ID of the father
##'   \item \code{name}:  Name of the person
##'   \item \code{sex}: Biological sex
##'   \item \code{twinID}:  ID of the twin, if applicable
##'   \item \code{zygosity}: Zygosity of the twin, if applicable. mz is monozygotic; dz is dizygotic
##'   }
##'
##' @docType data
##' @keywords datasets
##' @name ASOIAF
##' @usage data(ASOIAF)
##' @format A data frame with 503 observations
NULL
