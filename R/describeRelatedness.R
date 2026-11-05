#' Describe Genetic Relatedness with Textual Labels
#'
#' @description
#' This function takes a flat/vectorized relatedness matrix (as produced by
#' \code{\link{com2links}}) and generates textual descriptions of the genetic
#' relationships based on additive relatedness coefficients and generation differences.
#'
#' @param rel_df A data.frame with columns for pairwise relatedness information.
#'   Required columns: ID1, ID2 (or custom names via \code{ID1_col}/\code{ID2_col}).
#'   Typically produced by \code{\link{com2links}}.
#' @param ped Optional data.frame containing pedigree information. If provided, sex
#'   and generation information will be automatically joined. Should contain columns
#'   specified by \code{personID}, \code{sex_col}, and optionally \code{gen_col}.
#' @param personID Character. Name of the column in \code{ped} containing person IDs.
#'   Default is "personID".
#' @param sex_col Character. Name of the column in \code{ped} containing sex information.
#'   Default is "sex".
#' @param gen_col Character. Name of the column in \code{ped} containing generation numbers.
#'   If NULL (default), relationships will be described without generation information.
#' @param ID1_col Character. Name of the column in \code{rel_df} for person 1 ID.
#'   Default is "ID1".
#' @param ID2_col Character. Name of the column in \code{rel_df} for person 2 ID.
#'   Default is "ID2".
#' @param add_col Character. Name of the column containing additive relatedness coefficients.
#'   Default is "addRel".
#' @param code_male The value used to denote males in the sex column. Default is 1.
#' @param code_female The value used to denote females in the sex column. Default is 0.
#' @param use_sex Logical. If TRUE and sex information is available, generate sex-specific
#'   relationship labels (e.g., "mother-child" vs "father-child"). Default is FALSE.
#' @param return_list Logical. If TRUE, return a list with both the original data and
#'   the relationship descriptions. If FALSE, return a data.frame with an added
#'   "relationship" column. Default is FALSE.
#'
#' @details
#' The function uses the additive relatedness coefficient (\code{addRel}) and generation
#' difference to infer relationship types. Common relationships and their expected
#' values are:
#' \itemize{
#'   \item addRel = 1.0, same generation: "self" (same individual)
#'   \item addRel = 0.5, same generation: "full siblings"
#'   \item addRel = 0.5, 1 generation apart: "parent-child"
#'   \item addRel = 0.25, same generation: "half siblings"
#'   \item addRel = 0.25, 1 generation apart: "aunt/uncle-niece/nephew"
#'   \item addRel = 0.25, 2 generations apart: "grandparent-grandchild"
#'   \item addRel = 0.125, same generation: "first cousins"
#'   \item addRel = 0.0625, same generation: "second cousins"
#'   \item And relationships with various degrees of removal for cousins
#' }
#'
#' When \code{use_sex = TRUE} and sex information is available (either from \code{ped}
#' or already present in \code{rel_df}), parent-child relationships can be further
#' specified as "mother-child", "father-child", "mother-son", "mother-daughter",
#' "father-son", or "father-daughter". Similar sex-specific labels are provided for
#' grandparent and avuncular relationships.
#'
#' @return
#' If \code{return_list = FALSE} (default), returns a data.frame identical to \code{rel_df}
#' with an additional "relationship" column containing textual descriptions.
#' If \code{return_list = TRUE}, returns a list with two elements:
#' \itemize{
#'   \item \code{data}: The original data.frame with the relationship column added
#'   \item \code{relationships}: A character vector of relationship descriptions
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with com2links output
#' library(BGmisc)
#' data(potter)
#' 
#' # Compute additive relatedness matrix
#' add_matrix <- ped2add(potter, sparse = FALSE)
#' 
#' # Convert to pairwise format
#' rel_pairs <- com2links(ad_ped_matrix = add_matrix, writetodisk = FALSE)
#' 
#' # Describe relationships (without generation info)
#' described <- describeRelatedness(rel_pairs)
#' head(described)
#' 
#' # With pedigree for sex-specific labels
#' # (assumes potter has a 'gen' column for generation)
#' described_sex <- describeRelatedness(rel_pairs, ped = potter, 
#'                                      use_sex = TRUE, gen_col = "gen")
#' head(described_sex)
#' }
#'
#' @export
describeRelatedness <- function(rel_df,
                                ped = NULL,
                                personID = "personID",
                                sex_col = "sex",
                                gen_col = NULL,
                                ID1_col = "ID1",
                                ID2_col = "ID2",
                                add_col = "addRel",
                                code_male = 1,
                                code_female = 0,
                                use_sex = FALSE,
                                return_list = FALSE) {
  # Validate input
  if (!is.data.frame(rel_df)) {
    stop("rel_df must be a data.frame")
  }

  if (!ID1_col %in% names(rel_df) || !ID2_col %in% names(rel_df)) {
    stop(paste0("rel_df must contain '", ID1_col, "' and '", ID2_col, "' columns"))
  }

  if (!add_col %in% names(rel_df)) {
    stop(paste0("Column '", add_col, "' not found in rel_df"))
  }

  # If pedigree is provided, join sex and generation information
  if (!is.null(ped)) {
    if (!is.data.frame(ped)) {
      stop("ped must be a data.frame")
    }
    
    if (!personID %in% names(ped)) {
      stop(paste0("Column '", personID, "' not found in ped"))
    }
    
    # Determine which columns to extract from pedigree
    cols_needed <- personID
    if (use_sex && sex_col %in% names(ped)) {
      cols_needed <- c(cols_needed, sex_col)
    } else if (use_sex && !sex_col %in% names(ped)) {
      warning(paste0("use_sex is TRUE but '", sex_col, "' column not found in ped. Proceeding without sex-specific labels."))
      use_sex <- FALSE
    }
    
    if (!is.null(gen_col) && gen_col %in% names(ped)) {
      cols_needed <- c(cols_needed, gen_col)
    }
    
    # Extract only needed columns once
    ped_subset <- ped[, cols_needed, drop = FALSE]
    
    # Join for ID1
    ped_for_id1 <- ped_subset
    names(ped_for_id1)[1] <- ID1_col
    if (use_sex && sex_col %in% names(ped_subset)) {
      names(ped_for_id1)[names(ped_for_id1) == sex_col] <- "sex1"
    }
    if (!is.null(gen_col) && gen_col %in% names(ped_subset)) {
      names(ped_for_id1)[names(ped_for_id1) == gen_col] <- "gen1"
    }
    
    rel_df <- merge(rel_df, ped_for_id1, by = ID1_col, all.x = TRUE, sort = FALSE)
    
    # Join for ID2 - reuse the subset with renamed columns
    ped_for_id2 <- ped_subset
    names(ped_for_id2)[1] <- ID2_col
    if (use_sex && sex_col %in% names(ped_subset)) {
      names(ped_for_id2)[names(ped_for_id2) == sex_col] <- "sex2"
    }
    if (!is.null(gen_col) && gen_col %in% names(ped_subset)) {
      names(ped_for_id2)[names(ped_for_id2) == gen_col] <- "gen2"
    }
    
    rel_df <- merge(rel_df, ped_for_id2, by = ID2_col, all.x = TRUE, sort = FALSE)
  }

  # Check for generation columns (either from ped join or already in rel_df)
  has_gen <- "gen1" %in% names(rel_df) && "gen2" %in% names(rel_df)

  # Check for sex columns (either from ped join or already in rel_df)
  has_sex <- "sex1" %in% names(rel_df) && "sex2" %in% names(rel_df)

  if (use_sex && !has_sex) {
    warning("use_sex is TRUE but sex columns are not available. Proceeding without sex-specific labels.")
    use_sex <- FALSE
  }

  # Extract relatedness coefficients
  addRel <- rel_df[[add_col]]

  # Extract generation differences if available
  if (has_gen) {
    gen_diff <- rel_df[["gen2"]] - rel_df[["gen1"]]
    gen_abs_diff <- abs(gen_diff)
  } else {
    gen_diff <- rep(NA_real_, nrow(rel_df))
    gen_abs_diff <- rep(NA_real_, nrow(rel_df))
  }

  # Extract sex if needed
  if (use_sex && has_sex) {
    sex1 <- rel_df[["sex1"]]
    sex2 <- rel_df[["sex2"]]
  } else {
    sex1 <- rep(NA, nrow(rel_df))
    sex2 <- rep(NA, nrow(rel_df))
  }

  # Vectorized classification using lookup table
  relationships <- classify_relationships_vectorized(
    addRel = addRel,
    gen_diff = gen_diff,
    gen_abs_diff = gen_abs_diff,
    sex1 = sex1,
    sex2 = sex2,
    code_male = code_male,
    code_female = code_female,
    use_sex = use_sex
  )

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


#' Vectorized relationship classification using lookup tables
#'
#' @param addRel Numeric vector of additive relatedness coefficients
#' @param gen_diff Numeric vector of generation differences
#' @param gen_abs_diff Numeric vector of absolute generation differences
#' @param sex1 Vector of sex values for person 1
#' @param sex2 Vector of sex values for person 2
#' @param code_male Code for male
#' @param code_female Code for female
#' @param use_sex Logical. Whether to use sex-specific labels
#'
#' @return Character vector of relationship descriptions
#' @keywords internal
classify_relationships_vectorized <- function(addRel, gen_diff, gen_abs_diff, 
                                             sex1, sex2, code_male, code_female, use_sex) {
  n <- length(addRel)
  relationships <- character(n)
  tol <- 0.001
  
  # Create lookup table for basic relationships
  lookup_table <- create_relationship_lookup()
  
  # Vectorized processing
  for (i in seq_len(n)) {
    r <- addRel[i]
    g_abs <- gen_abs_diff[i]
    g_diff <- gen_diff[i]
    
    # Handle special cases first
    if (abs(r - 1.0) < tol) {
      relationships[i] <- if (!is.na(g_abs) && g_abs == 0) "self/identical twin" else "self"
      next
    }
    
    if (abs(r) < tol) {
      relationships[i] <- "unrelated"
      next
    }
    
    # Use lookup table for known relationships
    rel <- lookup_relationship(r, g_abs, tol, lookup_table)
    
    if (!is.na(rel)) {
      # Check if sex-specific label is needed
      if (use_sex && !is.na(sex1[i]) && !is.na(sex2[i]) && !is.na(g_diff)) {
        sex_specific <- get_sex_specific_label(rel, g_diff, sex1[i], sex2[i], 
                                               code_male, code_female)
        if (!is.null(sex_specific)) {
          relationships[i] <- sex_specific
          next
        }
      }
      relationships[i] <- rel
    } else {
      # Fallback for unknown relationships
      relationships[i] <- sprintf("unknown (r=%.4g)", r)
    }
  }
  
  relationships
}


#' Create relationship lookup table
#'
#' @return List structure mapping (r, gen_abs_diff) to relationship names
#' @keywords internal
create_relationship_lookup <- function() {
  list(
    # Format: r_value = list of (gen_abs_diff, relationship_name) pairs
    "1.0" = list(c(0, "self/identical twin")),
    "0.5" = list(
      c(0, "full siblings"),
      c(1, "parent-child")
    ),
    "0.25" = list(
      c(0, "half siblings"),
      c(1, "aunt/uncle-niece/nephew"),
      c(2, "grandparent-grandchild")
    ),
    "0.125" = list(
      c(0, "first cousins"),
      c(1, "first cousins once removed"),
      c(2, "first cousins twice removed"),
      c(3, "great-grandparent-great-grandchild")
    ),
    "0.0625" = list(
      c(0, "second cousins"),
      c(1, "second cousins once removed"),
      c(2, "second cousins twice removed"),
      c(4, "great-great-grandparent-great-great-grandchild")
    ),
    "0.03125" = list(
      c(0, "third cousins"),
      c(1, "third cousins once removed"),
      c(2, "third cousins twice removed")
    )
  )
}


#' Lookup relationship from table
#'
#' @param r Relatedness coefficient
#' @param gen_abs_diff Absolute generation difference
#' @param tol Tolerance for floating point comparison
#' @param lookup_table Lookup table created by create_relationship_lookup
#'
#' @return Relationship name or NA if not found
#' @keywords internal
lookup_relationship <- function(r, gen_abs_diff, tol, lookup_table) {
  if (is.na(gen_abs_diff)) {
    # Without generation info, only return coefficient-based label
    for (r_key in names(lookup_table)) {
      r_val <- as.numeric(r_key)
      if (abs(r - r_val) < tol) {
        return(sprintf("unknown (r=%.4g)", r))
      }
    }
    return(NA_character_)
  }
  
  # Search lookup table
  for (r_key in names(lookup_table)) {
    r_val <- as.numeric(r_key)
    if (abs(r - r_val) < tol) {
      # Found matching r value, now check generation difference
      entries <- lookup_table[[r_key]]
      for (entry in entries) {
        if (entry[1] == gen_abs_diff) {
          return(entry[2])
        }
      }
      # r matches but not the generation - could be a cousin n-times removed
      # Use generalized cousin naming
      if (r_val <= 0.125 && r_val >= 0.015625 && gen_abs_diff > 0) {
        return(generalize_cousin_relationship(r_val, gen_abs_diff, tol))
      }
    }
  }
  
  NA_character_
}


#' Generalize cousin relationships for arbitrary degrees of removal
#'
#' @param r Relatedness coefficient
#' @param gen_abs_diff Absolute generation difference
#' @param tol Tolerance for comparison
#'
#' @return Generalized relationship name
#' @keywords internal
generalize_cousin_relationship <- function(r, gen_abs_diff, tol) {
  # Determine cousin degree from relatedness coefficient
  # First cousins: 0.125 = 2^-3
  # Second cousins: 0.0625 = 2^-4
  # Third cousins: 0.03125 = 2^-5
  # Fourth cousins: 0.015625 = 2^-6
  
  degree_map <- c(0.125, 0.0625, 0.03125, 0.015625)
  degree_names <- c("first", "second", "third", "fourth")
  
  for (i in seq_along(degree_map)) {
    if (abs(r - degree_map[i]) < tol) {
      if (gen_abs_diff == 0) {
        return(paste(degree_names[i], "cousins"))
      } else if (gen_abs_diff == 1) {
        return(paste(degree_names[i], "cousins once removed"))
      } else if (gen_abs_diff == 2) {
        return(paste(degree_names[i], "cousins twice removed"))
      } else {
        times <- c("once", "twice", "thrice")
        if (gen_abs_diff <= 3) {
          times_word <- times[gen_abs_diff]
        } else {
          times_word <- paste(gen_abs_diff, "times")
        }
        return(paste(degree_names[i], "cousins", times_word, "removed"))
      }
    }
  }
  
  sprintf("unknown (r=%.4g)", r)
}


#' Get sex-specific relationship label
#'
#' @param base_rel Base relationship name
#' @param gen_diff Generation difference (gen2 - gen1)
#' @param sex1 Sex of person 1
#' @param sex2 Sex of person 2
#' @param code_male Code for male
#' @param code_female Code for female
#'
#' @return Sex-specific relationship name or NULL if not applicable
#' @keywords internal
get_sex_specific_label <- function(base_rel, gen_diff, sex1, sex2, code_male, code_female) {
  # Only provide sex-specific labels for certain relationship types
  if (base_rel == "parent-child") {
    return(get_parent_child_label(gen_diff, sex1, sex2, code_male, code_female))
  } else if (base_rel == "aunt/uncle-niece/nephew") {
    return(get_avuncular_label(gen_diff, sex1, sex2, code_male, code_female))
  } else if (base_rel == "grandparent-grandchild") {
    return(get_grandparent_label(gen_diff, sex1, sex2, code_male, code_female))
  }
  
  NULL
}


#' Classify a single relationship based on relatedness and generation (DEPRECATED)
#'
#' This function is kept for backward compatibility but is no longer used internally.
#' Use classify_relationships_vectorized instead.
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
  # Deprecated: Use classify_relationships_vectorized instead
  # This wrapper is kept for backward compatibility
  result <- classify_relationships_vectorized(
    addRel = r,
    gen_diff = gen_diff,
    gen_abs_diff = gen_abs_diff,
    sex1 = sex1,
    sex2 = sex2,
    code_male = code_male,
    code_female = code_female,
    use_sex = use_sex
  )
  result[1]
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
  # Create lookup key
  parent_is_1 <- gen_diff > 0
  
  # Lookup table: (parent_sex, child_sex) -> label
  labels <- list(
    parent_child = list(
      list(code_female, code_female, "mother-daughter"),
      list(code_female, code_male, "mother-son"),
      list(code_male, code_female, "father-daughter"),
      list(code_male, code_male, "father-son")
    ),
    child_parent = list(
      list(code_female, code_female, "daughter-mother"),
      list(code_male, code_female, "son-mother"),
      list(code_female, code_male, "daughter-father"),
      list(code_male, code_male, "son-father")
    )
  )
  
  if (parent_is_1) {
    # Person 1 is the parent
    for (combo in labels$parent_child) {
      if (sex1 == combo[[1]] && sex2 == combo[[2]]) {
        return(combo[[3]])
      }
    }
    # Fallback with partial info
    if (sex1 == code_female) return("mother-child")
    if (sex1 == code_male) return("father-child")
  } else {
    # Person 2 is the parent
    for (combo in labels$child_parent) {
      if (sex1 == combo[[1]] && sex2 == combo[[2]]) {
        return(combo[[3]])
      }
    }
    # Fallback with partial info
    if (sex2 == code_female) return("child-mother")
    if (sex2 == code_male) return("child-father")
  }
  
  "parent-child"
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
  # Lookup table approach
  elder_is_1 <- gen_diff > 0
  
  labels <- list(
    elder_younger = list(
      list(code_female, code_female, "aunt-niece"),
      list(code_female, code_male, "aunt-nephew"),
      list(code_male, code_female, "uncle-niece"),
      list(code_male, code_male, "uncle-nephew")
    ),
    younger_elder = list(
      list(code_female, code_female, "niece-aunt"),
      list(code_male, code_female, "nephew-aunt"),
      list(code_female, code_male, "niece-uncle"),
      list(code_male, code_male, "nephew-uncle")
    )
  )
  
  label_list <- if (elder_is_1) labels$elder_younger else labels$younger_elder
  
  for (combo in label_list) {
    if (sex1 == combo[[1]] && sex2 == combo[[2]]) {
      return(combo[[3]])
    }
  }
  
  "aunt/uncle-niece/nephew"
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
  # Lookup table approach
  elder_is_1 <- gen_diff > 0
  
  labels <- list(
    grand_grand = list(
      list(code_female, code_female, "grandmother-granddaughter"),
      list(code_female, code_male, "grandmother-grandson"),
      list(code_male, code_female, "grandfather-granddaughter"),
      list(code_male, code_male, "grandfather-grandson")
    ),
    grand_grand_reverse = list(
      list(code_female, code_female, "granddaughter-grandmother"),
      list(code_male, code_female, "grandson-grandmother"),
      list(code_female, code_male, "granddaughter-grandfather"),
      list(code_male, code_male, "grandson-grandfather")
    )
  )
  
  label_list <- if (elder_is_1) labels$grand_grand else labels$grand_grand_reverse
  
  for (combo in label_list) {
    if (sex1 == combo[[1]] && sex2 == combo[[2]]) {
      return(combo[[3]])
    }
  }
  
  # Fallback with partial info
  if (elder_is_1) {
    if (sex1 == code_female) return("grandmother-grandchild")
    if (sex1 == code_male) return("grandfather-grandchild")
  } else {
    if (sex2 == code_female) return("grandchild-grandmother")
    if (sex2 == code_male) return("grandchild-grandfather")
  }
  
  "grandparent-grandchild"
}
