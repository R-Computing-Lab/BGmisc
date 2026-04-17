#' @rdname simulatePedigree
#' @export
SimPed <- function(...) { # nolint: object_name_linter.
  warning("The 'SimPed' function is deprecated. Please use 'simulatePedigree' instead.")
  simulatePedigree(...)
}
