##' Artificial pedigree data on seven families with inbreeding
##'
##' A dataset created purely from imagination that includes several types of inbreeding.
##' Different kinds of inbreeding occur in each extended family.
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
##' @format A data frame (and ped object) with 113 rows and 7 variables
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
