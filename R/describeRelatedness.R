#' Describe Genetic Relatedness with Textual Labels
#'
#' @description
#' This function takes a flat/vectorized relatedness matrix and generates textual
#' descriptions of the genetic relationships based on additive relatedness coefficients
#' and generation differences.
#'
#' @param rel_df A data.frame with columns for pairwise relatedness information.
#'   Required columns: ID1, ID2, addRel (additive relatedness coefficient).
#'   Optional columns: gen1, gen2 (generation numbers), sex1, sex2 (biological sex).
#' @param add_col Character. Name of the column containing additive relatedness coefficients.
#'   Default is "addRel".
#' @param gen1_col Character. Name of the column containing generation for person 1.
#'   Default is "gen1".
#' @param gen2_col Character. Name of the column containing generation for person 2.
#'   Default is "gen2".
#' @param sex1_col Character. Name of the column containing sex for person 1.
#'   Default is "sex1".
#' @param sex2_col Character. Name of the column containing sex for person 2.
#'   Default is "sex2".
#' @param code_male The value used to denote males. Default is 1.
#' @param code_female The value used to denote females. Default is 0.
#' @param use_sex Logical. If TRUE and sex columns are available, generate sex-specific
#'   relationship labels (e.g., "mother-child" vs "father-child"). Default is FALSE.
#' @param return_list Logical. If TRUE, return a list with both the original data and
#'   the relationship descriptions. If FALSE, return a data.frame with an added
#'   "relationship" column. Default is FALSE.
#'
#' @details
#' The function uses the additive relatedness coefficient (`addRel`) and generation
#' difference to infer relationship types. Common relationships and their expected
#' values are:
#' \itemize{
#'   \item addRel = 1.0, gen1 == gen2: "self" (same individual)
#'   \item addRel = 0.5, gen1 == gen2: "full siblings"
#'   \item addRel = 0.5, |gen1 - gen2| == 1: "parent-child"
#'   \item addRel = 0.25, gen1 == gen2: "half siblings"
#'   \item addRel = 0.25, |gen1 - gen2| == 1: "aunt/uncle-niece/nephew"
#'   \item addRel = 0.25, |gen1 - gen2| == 2: "grandparent-grandchild"
#'   \item addRel = 0.125, gen1 == gen2: "first cousins"
#'   \item addRel = 0.0625, gen1 == gen2: "second cousins"
#'   \item And relationships with various degrees of removal for cousins
#' }
#'
#' When `use_sex = TRUE` and sex information is available, parent-child relationships
#' can be further specified as "mother-child", "father-child", "mother-son",
#' "mother-daughter", "father-son", or "father-daughter".
#'
#' @return
#' If `return_list = FALSE` (default), returns a data.frame identical to `rel_df`
#' with an additional "relationship" column containing textual descriptions.
#' If `return_list = TRUE`, returns a list with two elements:
#' \itemize{
#'   \item `data`: The original data.frame with the relationship column added
#'   \item `relationships`: A character vector of relationship descriptions
#' }
#'
#' @examples
#' \dontrun{
#' # Create example relatedness data
#' rel_data <- data.frame(
#'   ID1 = c(1, 1, 1, 2),
#'   ID2 = c(2, 3, 4, 3),
#'   addRel = c(0.5, 0.25, 0.125, 0.5),
#'   gen1 = c(1, 1, 1, 2),
#'   gen2 = c(2, 3, 1, 2)
#' )
#'
#' # Get relationship descriptions
#' described <- describeRelatedness(rel_data)
#' print(described)
#'
#' # With sex-specific labels
#' rel_data$sex1 <- c(0, 0, 1, 1)
#' rel_data$sex2 <- c(1, 0, 0, 1)
#' described_sex <- describeRelatedness(rel_data, use_sex = TRUE)
#' print(described_sex)
#' }
#'
#' @export
describeRelatedness <- function(rel_df,
                                add_col = "addRel",
                                gen1_col = "gen1",
                                gen2_col = "gen2",
                                sex1_col = "sex1",
                                sex2_col = "sex2",
                                code_male = 1,
                                code_female = 0,
                                use_sex = FALSE,
                                return_list = FALSE) {
  # Validate input
  if (!is.data.frame(rel_df)) {
    stop("rel_df must be a data.frame")
  }

  if (!"ID1" %in% names(rel_df) || !"ID2" %in% names(rel_df)) {
    stop("rel_df must contain 'ID1' and 'ID2' columns")
  }

  if (!add_col %in% names(rel_df)) {
    stop(paste0("Column '", add_col, "' not found in rel_df"))
  }

  # Check for generation columns
  has_gen <- gen1_col %in% names(rel_df) && gen2_col %in% names(rel_df)

  # Check for sex columns
  has_sex <- sex1_col %in% names(rel_df) && sex2_col %in% names(rel_df)

  if (use_sex && !has_sex) {
    warning("use_sex is TRUE but sex columns are not available. Proceeding without sex-specific labels.")
    use_sex <- FALSE
  }

  # Extract relatedness coefficients
  addRel <- rel_df[[add_col]]

  # Extract generation differences if available
  if (has_gen) {
    gen1 <- rel_df[[gen1_col]]
    gen2 <- rel_df[[gen2_col]]
    gen_diff <- gen2 - gen1  # Positive means person 2 is in a younger generation
    gen_abs_diff <- abs(gen_diff)
  } else {
    gen1 <- NULL
    gen2 <- NULL
    gen_diff <- NULL
    gen_abs_diff <- NULL
  }

  # Extract sex if needed
  if (use_sex && has_sex) {
    sex1 <- rel_df[[sex1_col]]
    sex2 <- rel_df[[sex2_col]]
  } else {
    sex1 <- NULL
    sex2 <- NULL
  }

  # Initialize relationship vector
  n <- nrow(rel_df)
  relationships <- character(n)

  # Classify each relationship
  for (i in 1:n) {
    r <- addRel[i]
    relationships[i] <- classify_relationship(
      r = r,
      gen_diff = if (has_gen) gen_diff[i] else NA,
      gen_abs_diff = if (has_gen) gen_abs_diff[i] else NA,
      sex1 = if (use_sex) sex1[i] else NA,
      sex2 = if (use_sex) sex2[i] else NA,
      code_male = code_male,
      code_female = code_female,
      use_sex = use_sex
    )
  }

  # Prepare output
  result_df <- rel_df
  result_df$relationship <- relationships

  if (return_list) {
    return(list(
      data = result_df,
      relationships = relationships
    ))
  } else {
    return(result_df)
  }
}


#' Classify a single relationship based on relatedness and generation
#'
#' @param r Numeric. Additive relatedness coefficient
#' @param gen_diff Numeric. Generation difference (gen2 - gen1)
#' @param gen_abs_diff Numeric. Absolute generation difference
#' @param sex1 Sex of person 1
#' @param sex2 Sex of person 2
#' @param code_male Code for male
#' @param code_female Code for female
#' @param use_sex Logical. Whether to use sex-specific labels
#'
#' @return Character string describing the relationship
#' @keywords internal
classify_relationship <- function(r, gen_diff, gen_abs_diff, sex1, sex2,
                                  code_male, code_female, use_sex) {
  # Tolerance for floating point comparison
  tol <- 0.001

  # Handle self (same person or identical twins)
  if (abs(r - 1.0) < tol) {
    if (!is.na(gen_abs_diff) && gen_abs_diff == 0) {
      return("self/identical twin")
    }
    return("self")
  }

  # Handle unrelated (r = 0)
  if (abs(r) < tol) {
    return("unrelated")
  }

  # For relationships with generation information
  if (!is.na(gen_abs_diff) && !is.na(gen_diff)) {
    # Same generation relationships
    if (gen_abs_diff == 0) {
      if (abs(r - 0.5) < tol) {
        return("full siblings")
      } else if (abs(r - 0.25) < tol) {
        return("half siblings")
      } else if (abs(r - 0.125) < tol) {
        return("first cousins")
      } else if (abs(r - 0.0625) < tol) {
        return("second cousins")
      } else if (abs(r - 0.03125) < tol) {
        return("third cousins")
      }
    }

    # One generation apart
    if (gen_abs_diff == 1) {
      if (abs(r - 0.5) < tol) {
        # Parent-child relationship
        if (use_sex && !is.na(sex1) && !is.na(sex2)) {
          return(get_parent_child_label(gen_diff, sex1, sex2, code_male, code_female))
        }
        return("parent-child")
      } else if (abs(r - 0.25) < tol) {
        # Aunt/uncle - niece/nephew
        if (use_sex && !is.na(sex1) && !is.na(sex2)) {
          return(get_avuncular_label(gen_diff, sex1, sex2, code_male, code_female))
        }
        return("aunt/uncle-niece/nephew")
      } else if (abs(r - 0.125) < tol) {
        return("first cousins once removed")
      } else if (abs(r - 0.0625) < tol) {
        return("second cousins once removed")
      }
    }

    # Two generations apart
    if (gen_abs_diff == 2) {
      if (abs(r - 0.25) < tol) {
        if (use_sex && !is.na(sex1) && !is.na(sex2)) {
          return(get_grandparent_label(gen_diff, sex1, sex2, code_male, code_female))
        }
        return("grandparent-grandchild")
      } else if (abs(r - 0.125) < tol) {
        return("first cousins twice removed")
      } else if (abs(r - 0.0625) < tol) {
        return("second cousins twice removed")
      }
    }

    # Three generations apart
    if (gen_abs_diff == 3) {
      if (abs(r - 0.125) < tol) {
        return("great-grandparent-great-grandchild")
      }
    }

    # Four generations apart
    if (gen_abs_diff == 4) {
      if (abs(r - 0.0625) < tol) {
        return("great-great-grandparent-great-great-grandchild")
      }
    }
  }

  # Fallback: describe by coefficient only if we can't determine specific relationship
  if (abs(r - 0.5) < tol) {
    return("unknown (r=0.5)")
  } else if (abs(r - 0.25) < tol) {
    return("unknown (r=0.25)")
  } else if (abs(r - 0.125) < tol) {
    return("unknown (r=0.125)")
  } else if (abs(r - 0.0625) < tol) {
    return("unknown (r=0.0625)")
  } else {
    return(paste0("unknown (r=", round(r, 4), ")"))
  }
}


#' Get sex-specific parent-child relationship label
#'
#' @param gen_diff Generation difference
#' @param sex1 Sex of person 1
#' @param sex2 Sex of person 2
#' @param code_male Code for male
#' @param code_female Code for female
#'
#' @return Character string
#' @keywords internal
get_parent_child_label <- function(gen_diff, sex1, sex2, code_male, code_female) {
  # gen_diff > 0 means person 1 is parent (older generation)
  # gen_diff < 0 means person 2 is parent (older generation)

  if (gen_diff > 0) {
    # Person 1 is the parent
    if (sex1 == code_female && sex2 == code_male) {
      return("mother-son")
    } else if (sex1 == code_female && sex2 == code_female) {
      return("mother-daughter")
    } else if (sex1 == code_male && sex2 == code_male) {
      return("father-son")
    } else if (sex1 == code_male && sex2 == code_female) {
      return("father-daughter")
    } else if (sex1 == code_female) {
      return("mother-child")
    } else if (sex1 == code_male) {
      return("father-child")
    }
  } else {
    # Person 2 is the parent
    if (sex2 == code_female && sex1 == code_male) {
      return("son-mother")
    } else if (sex2 == code_female && sex1 == code_female) {
      return("daughter-mother")
    } else if (sex2 == code_male && sex1 == code_male) {
      return("son-father")
    } else if (sex2 == code_male && sex1 == code_female) {
      return("daughter-father")
    } else if (sex2 == code_female) {
      return("child-mother")
    } else if (sex2 == code_male) {
      return("child-father")
    }
  }
  return("parent-child")
}


#' Get sex-specific avuncular relationship label
#'
#' @param gen_diff Generation difference
#' @param sex1 Sex of person 1
#' @param sex2 Sex of person 2
#' @param code_male Code for male
#' @param code_female Code for female
#'
#' @return Character string
#' @keywords internal
get_avuncular_label <- function(gen_diff, sex1, sex2, code_male, code_female) {
  if (gen_diff > 0) {
    # Person 1 is the aunt/uncle
    if (sex1 == code_female && sex2 == code_male) {
      return("aunt-nephew")
    } else if (sex1 == code_female && sex2 == code_female) {
      return("aunt-niece")
    } else if (sex1 == code_male && sex2 == code_male) {
      return("uncle-nephew")
    } else if (sex1 == code_male && sex2 == code_female) {
      return("uncle-niece")
    }
  } else {
    # Person 2 is the aunt/uncle
    if (sex2 == code_female && sex1 == code_male) {
      return("nephew-aunt")
    } else if (sex2 == code_female && sex1 == code_female) {
      return("niece-aunt")
    } else if (sex2 == code_male && sex1 == code_male) {
      return("nephew-uncle")
    } else if (sex2 == code_male && sex1 == code_female) {
      return("niece-uncle")
    }
  }
  return("aunt/uncle-niece/nephew")
}


#' Get sex-specific grandparent relationship label
#'
#' @param gen_diff Generation difference
#' @param sex1 Sex of person 1
#' @param sex2 Sex of person 2
#' @param code_male Code for male
#' @param code_female Code for female
#'
#' @return Character string
#' @keywords internal
get_grandparent_label <- function(gen_diff, sex1, sex2, code_male, code_female) {
  if (gen_diff > 0) {
    # Person 1 is the grandparent
    if (sex1 == code_female && sex2 == code_male) {
      return("grandmother-grandson")
    } else if (sex1 == code_female && sex2 == code_female) {
      return("grandmother-granddaughter")
    } else if (sex1 == code_male && sex2 == code_male) {
      return("grandfather-grandson")
    } else if (sex1 == code_male && sex2 == code_female) {
      return("grandfather-granddaughter")
    } else if (sex1 == code_female) {
      return("grandmother-grandchild")
    } else if (sex1 == code_male) {
      return("grandfather-grandchild")
    }
  } else {
    # Person 2 is the grandparent
    if (sex2 == code_female && sex1 == code_male) {
      return("grandson-grandmother")
    } else if (sex2 == code_female && sex1 == code_female) {
      return("granddaughter-grandmother")
    } else if (sex2 == code_male && sex1 == code_male) {
      return("grandson-grandfather")
    } else if (sex2 == code_male && sex1 == code_female) {
      return("granddaughter-grandfather")
    } else if (sex2 == code_female) {
      return("grandchild-grandmother")
    } else if (sex2 == code_male) {
      return("grandchild-grandfather")
    }
  }
  return("grandparent-grandchild")
}
