##' Artificial pedigree data on eight families with inbreeding
##'
##' A dataset created purely from imagination that includes several types of inbreeding.
##' Different kinds of inbreeding occur in each extended family.
##' 
##' The types of inbreeding are as follows:
##' 
##' \itemize{
##'     \item{Extended Family 1}{Sister wives: Children with the same father and different mothers who are sisters}
##'     \item{Extended Family 2}{Full siblings have children}
##'     \item{Extended Family 3}{Half siblings have children}
##'     \item{Extended Family 4}{First cousins have children}
##'     \item{Extended Family 5}{Father has child with his daughter}
##'     \item{Extended Family 6}{Half sister wives: Children with the same father and different mothers who are half sisters}
##'     \item{Extended Family 7}{Uncle-niece and Aunt-nephew have children}
##'     \item{Extended Family 8}{A father-son pairs has children with a corresponding mother-daughter pair}
##' }
##' 
##' Although not all of the above structures are technically inbreeding, they aim to test pedigree diagramming and path tracing algorithms.
##'
##' The variables are as follows:
##'
##' \itemize{
##'   \item ID.  Person identification variable
##'   \item sex.  Sex of the ID: 1 is female; 0 is male
##'   \item dadID.  ID of the father
##'   \item momID.  ID of the mother
##'   \item FamID.  ID of the extended family
##'   \item Gen.  Generation of the person
##'   \item proband.  Always FALSE
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
##'   \item FamID.  ID of the extended family
##'   \item ID.  Person identification variable
##'   \item sex.  Sex of the ID: 1 is female; 0 is male
##'   \item dadID.  ID of the father
##'   \item momID.  ID of the mother
##'   \item affected.  logical.  Whether the person is affected or not
##'   \item DA1.  Binary variable signifying the meaninglessness of life
##'   \item DA2.  Binary variable signifying the fundamental unknowability of existence
##'   \item birthYr.  Birth year for person
##'   \item onsetYr.  Year of onset for person
##'   \item deathYr.  Death year for person
##'   \item available.  logical.  Whether
##'   \item Gen.  Generation of the person
##'   \item proband.  logical.  Whether the person is a proband or not
##' }
##'
##' @docType data
##' @keywords datasets
##' @name hazard
##' @usage data(hazard)
##' @format A data frame with 43 rows and 14 variables
NULL
