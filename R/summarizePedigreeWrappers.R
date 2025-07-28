#' Summarize the maternal lines in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export
#' @aliases summarizematrilines summarisematrilines summariseMatrilines
summarizeMatrilines <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL, include_founder = FALSE,
                                founder_sort_var = NULL,
                                n_biggest = 5, n_oldest = 5, skip_var = NULL,
                                five_num_summary = FALSE, verbose = FALSE,
                                network_checks = FALSE) {
  # Call to wrapper function
  summarizePedigrees(
    ped = ped,
    personID = personID,
    n_biggest = n_biggest,
    n_oldest = n_oldest,
    byr = byr,
    include_founder = include_founder,
    momID = momID, dadID = dadID,
    famID = famID, matID = matID, patID = patID,
    skip_var = skip_var,
    type = "mothers", verbose = verbose,
    five_num_summary = five_num_summary,
    founder_sort_var = founder_sort_var,
    network_checks = network_checks
  )
}




#' Summarize the paternal lines in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @export
#' @aliases summarizepatrilines summarisepatrilines summarisePatrilines
summarizePatrilines <- function(ped, famID = "famID", personID = "ID",
                                momID = "momID", dadID = "dadID",
                                matID = "matID", patID = "patID",
                                byr = NULL, founder_sort_var = NULL,
                                include_founder = FALSE,
                                n_biggest = 5, n_oldest = 5, skip_var = NULL,
                                five_num_summary = FALSE, verbose = FALSE,
                                network_checks = FALSE) {
  # Call to wrapper function
  summarizePedigrees(
    ped = ped,
    personID = personID,
    n_biggest = n_biggest,
    n_oldest = n_oldest,
    byr = byr,
    include_founder = include_founder,
    momID = momID, dadID = dadID,
    famID = famID, matID = matID, patID = patID, skip_var = skip_var,
    type = "fathers", verbose = verbose, five_num_summary = five_num_summary,
    founder_sort_var = founder_sort_var, network_checks = network_checks
  )
}

#' Summarize the families in a pedigree
#' @inheritParams summarizePedigrees
#' @seealso [summarizePedigrees ()]
#' @aliases summarizefamilies summarisefamilies summariseFamilies
#' @export
summarizeFamilies <- function(ped, famID = "famID", personID = "ID",
                              momID = "momID", dadID = "dadID",
                              matID = "matID", patID = "patID",
                              byr = NULL, founder_sort_var = NULL,
                              include_founder = FALSE,
                              n_biggest = 5, n_oldest = 5, skip_var = NULL,
                              five_num_summary = FALSE, verbose = FALSE,
                              network_checks = FALSE) {
  # Call to wrapper function
  summarizePedigrees(
    ped = ped,
    personID = personID,
    n_biggest = n_biggest,
    n_oldest = n_oldest,
    byr = byr,
    include_founder = include_founder,
    momID = momID,
    dadID = dadID,
    famID = famID,
    matID = matID,
    patID = patID,
    skip_var = skip_var,
    type = "families",
    verbose = verbose,
    five_num_summary = five_num_summary,
    founder_sort_var = founder_sort_var,
    network_checks = network_checks
  )
}


#' @rdname summarizePedigrees
#' @export
summarisePedigrees <- summarizePedigrees

#' @rdname summarizeFamilies
#' @export
summariseFamilies <- summarizeFamilies

#' @rdname summarizeMatrilines
#' @export
summariseMatrilines <- summarizeMatrilines

#' @rdname summarizePatrilines
#' @export
summarisePatrilines <- summarizePatrilines
