#' Confidence Intervals for Correlations with Optional Design-Effect Adjustment
#'
#' @description
#' Compute confidence intervals (CIs) for correlation coefficients using either
#' Fisher's \eqn{r \rightarrow z} approach (Raykov-style on the \eqn{z} scale) or
#' a Wald CI on the \eqn{r} scale. Standard errors are first **adjusted** by a
#' design-effect factor when available, and optionally for double entry.
#' The adjusted standard errors are used for all calculations, including CIs, z-tests, and p-values.
#'
#'
#' @param tbl A data frame or tibble containing the correlation coefficient and standard error variables.
#' @param rho_var The name of the column in \code{tbl} that contains the correlation coefficients.
#' @param se_var The name of the column in \code{tbl} that contains the standard errors.
#' @param doubleentered Logical. If \code{TRUE}, the function assumes that the correlation coefficients are double-entered, which adjusts the standard errors accordingly. Default is \code{FALSE}.
#' @param method Character; CI method selector. Supported values:
#'   \itemize{
#'     \item \code{"raykov"} — Fisher \eqn{r \rightarrow z} CI (back-transformed).
#'     \item \code{"fisherz"} — alias of \code{"raykov"}.
#'     \item \code{"wald"} — Wald CI on the \eqn{r} scale.
#'     \item \code{"doubleentered"} — like \code{"raykov"} and, if
#'           \code{doubleentered} was not explicitly provided, it is set to
#'           \code{TRUE} (applies the \eqn{\sqrt{2}} multiplier).
#'     \item \code{"doubleenteredconserv"} — like \code{"wald"} and, if
#'           \code{doubleentered} was not explicitly provided, it is set to
#'           \code{TRUE}.
#'   }
#'
#' @param conf_level The confidence level for the intervals. Default is 0.95.
#' @param adjust_base A numeric value to adjust the standard errors. Default is 1.
#' @param design_effect_m A numeric value for the design effect related to the mean. Default is \code{NULL}.
#' @param design_effect_rho A numeric value for the design effect related to the correlation. Default is \code{NULL}.
#' @param design_effect_m_col A character string specifying the column name for the design effect related to the mean. Default is \code{NULL}.
#' @param design_effect_rho_col A character string specifying the column name for the design effect related to the correlation. Default is \code{NULL}.
#' @return A modified version of \code{tbl} with additional columns for the confidence intervals and related statistics.
#' Everything uses adjusted standard errors, including confidence intervals, z-tests, and p-values.
#' @examples
#' tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
#' calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "raykov")
#'
#' @export
#' @importFrom stats qnorm pnorm
#' @note Double-entry handling and design effects are governed by
#'   \code{doubleentered}, \code{design_effect_m}/\code{design_effect_rho}
#'   (or their \code{*_col} variants), and \code{adjust_base}. The
#'   \code{"doubleentered*"} method values simply provide convenient aliases:
#'   they toggle \code{doubleentered} to \code{TRUE} only when the user hasn't
#'   explicitly set it, and map to \code{"raykov"} or \code{"wald"} as described.
calculateCIs <- function(tbl,
                         rho_var,
                         se_var,
                         doubleentered = FALSE,
                         method = "raykov",
                         adjust_base = 1,
                         design_effect_m = NULL,
                         design_effect_rho = NULL,
                         design_effect_m_col = NULL,
                         design_effect_rho_col = NULL,
                         conf_level = 0.95) {
  # Load necessary packages
  # Normalize and interpret `method`, preserving user intent for `doubleentered`
  mc <- match.call()
  user_set_doubleentered <- "doubleentered" %in% names(mc)

  method_in <- tolower(method %||% "raykov") # `%||%` if you have it; else just tolower(method)
  method_effective <- switch(method_in,
    "raykov" = "raykov",
    "fisherz" = "raykov", # alias
    "wald" = "wald",
    "doubleentered" = {
      if (!user_set_doubleentered) doubleentered <- TRUE
      "raykov" # double-entry + Fisher z
    },
    "doubleenteredconserv" = {
      if (!user_set_doubleentered) doubleentered <- TRUE
      "wald" # double-entry + Wald (more conservative)
    },
    {
      warning(sprintf("Unrecognized method '%s'; defaulting to 'wald'.", method),
        call. = FALSE
      )
      "wald"
    }
  )


  # Get the name of the rho column regardless of input type
  if (mode(rho_var) != "character") {
    stop("rho_var must be a character string")
  }
  # Convert the rho_var and se_var into column names
  rho_col_name <- if (is.character(rho_var) && length(rho_var) == 1) {
    rho_var
  } else {
    deparse(substitute(rho_var))
  }

  se_col_name <- if (is.character(se_var) && length(se_var) == 1) {
    se_var
  } else {
    deparse(substitute(se_var))
  }


  # Construct new column names using paste0
  plusse_col_name <- paste0(rho_col_name, "_plusse")
  minusse_col_name <- paste0(rho_col_name, "_minusse")
  z_col_name <- paste0(rho_col_name, "_z")
  wald_col_name <- paste0(rho_col_name, "_wald")
  ztest_col_name <- paste0(rho_col_name, "_ztest")
  zp1tail_col_name <- paste0(rho_col_name, "_zp1tail")
  zp2tail_col_name <- paste0(rho_col_name, "_zp2tail")
  sez_col_name <- paste0(se_col_name, "_sez")
  se_adjusted_col_name <- paste0(se_col_name, "_se_adjusted")
  sez_adjusted_col_name <- paste0(se_col_name, "_sez_adjusted")
  waldp1tail_col_name <- paste0(rho_col_name, "_waldp1tail")
  waldp2tail_col_name <- paste0(rho_col_name, "_waldp2tail")

  # Create a copy of tbl to avoid modifying the original data frame
  tbl_out <- tbl
  n_rows <- nrow(tbl_out)

  # Resolve m input
  if (exists("design_effect_m_col") && length(design_effect_m_col) > 0) {
    m_vals <- tbl_out[[design_effect_m_col]]
  } else if (exists("design_effect_m") && length(design_effect_m) > 0) {
    if (length(design_effect_m) == 1) {
      design_effect_m <- rep(as.numeric(design_effect_m), n_rows)
    }
    m_vals <- design_effect_m
  } else {
    m_vals <- NULL
  }

  # Resolve rho input
  if (exists("design_effect_rho_col") && length(design_effect_rho_col) > 0) {
    rho_vals <- tbl_out[[design_effect_rho_col]]
  } else if (exists("design_effect_rho") && length(design_effect_rho) > 0) {
    if (length(design_effect_rho) == 1) {
      design_effect_rho <- rep(as.numeric(design_effect_rho), n_rows)
    }
    rho_vals <- design_effect_rho
  } else {
    rho_vals <- NULL
  }
  # Compute design_effect based on what is available
  if (!is.null(m_vals) && !is.null(rho_vals)) {
    design_effect <- sqrt(1 + (as.numeric(m_vals) - 1) * as.numeric(rho_vals))
  } else {
    design_effect <- rep(adjust_base, n_rows)
  }
  if (
    isTRUE(doubleentered)
  ) {
    # Adjust standard errors for double entry
    design_effect <- design_effect * sqrt(2)
  }
  z_crit <- stats::qnorm((1 + conf_level) / 2)

  if (method_effective == "raykov") {
    # Apply Fisher's r to z transform
    z_vals <- .fisherz(tbl_out[[rho_col_name]])
    sez_vals <- tbl_out[[se_col_name]] / (1 - tbl_out[[rho_col_name]]^2)

    tbl_out[[z_col_name]] <- z_vals
    tbl_out[[sez_col_name]] <- sez_vals
    tbl_out[[se_adjusted_col_name]] <- tbl_out[[se_col_name]] * design_effect
    tbl_out[[sez_adjusted_col_name]] <- tbl_out[[se_adjusted_col_name]] / (1 - tbl_out[[rho_col_name]]^2)
    tbl_out[[plusse_col_name]] <- .fisherz2r(z_vals + z_crit * tbl_out[[sez_adjusted_col_name]])
    tbl_out[[minusse_col_name]] <- .fisherz2r(z_vals - z_crit * tbl_out[[sez_adjusted_col_name]])

    # H0: r = 0. Compute z-test and p-values
    ztest_vals <- tbl_out[[z_col_name]] / tbl_out[[sez_adjusted_col_name]]

    tbl_out[[ztest_col_name]] <- ztest_vals
    tbl_out[[zp1tail_col_name]] <- stats::pnorm(q = abs(ztest_vals), lower.tail = FALSE)
    tbl_out[[zp2tail_col_name]] <- 2 * tbl_out[[zp1tail_col_name]]
  } else {
    tbl_out[[se_adjusted_col_name]] <- tbl_out[[se_col_name]] * design_effect

    # Compute confidence intervals
    tbl_out[[plusse_col_name]] <- tbl_out[[rho_col_name]] + z_crit * tbl_out[[se_adjusted_col_name]]
    tbl_out[[minusse_col_name]] <- tbl_out[[rho_col_name]] - z_crit * tbl_out[[se_adjusted_col_name]]
  }
  # Compute Wald statistics
  tbl_out[[wald_col_name]] <- tbl_out[[rho_col_name]] / tbl_out[[se_adjusted_col_name]]
  tbl_out[[waldp1tail_col_name]] <- stats::pnorm(q = abs(tbl_out[[wald_col_name]]), lower.tail = FALSE)
  tbl_out[[waldp2tail_col_name]] <- 2 * tbl_out[[waldp1tail_col_name]]

  return(tbl_out)
}
#' Fisher's r to z transformation and back
#'
#' These functions convert correlation coefficients (r) to Fisher's z-scores and back.
#' @param rho A numeric vector of correlation coefficients.
#' @return A numeric vector of transformed values.
#' @keywords internal
#' @details
#' Credit to the psych package for the Fisher's r to z transformation.

.fisherz <- function(rho) {
  0.5 * log((1 + rho) / (1 - rho))
} # converts r to z


#' @inherit .fisherz title
#' @inherit .fisherz return
#' @param z A numeric vector of Fisher's z-scores.
#' @keywords internal
#'
.fisherz2r <- function(z) {
  (exp(2 * z) - 1) / (1 + exp(2 * z))
} # converts back again
