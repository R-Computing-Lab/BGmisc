#' Read Wiki Family Tree
#'
#' @param text A character string containing the text of a family tree in wiki format.
#' @param verbose A logical value indicating whether to print messages.
#' @param file_path The path to the file containing the family tree.
#' @param ... Additional arguments (not used).
#'
#' @return A list containing the summary, members, structure, and relationships of the family tree.
#' @export
readWikifamilytree <- function(text = NULL, verbose = FALSE, file_path = NULL, ...) {
  # Checks
  if (is.null(text) && is.null(file_path)) {
    stop("Either 'text' or 'file_path' must be provided.")
  }
  # read from file if provided
  if (!is.null(file_path)) {
    if (!file.exists(file_path)) stop("File does not exist: ", file_path)

    if (verbose) {
      print(paste("Reading file:", file_path))
    }
    file <- data.frame(X1 = readLines(file_path))
    file_length <- nrow(file)
    if (verbose) {
      print(paste0("File is ", file_length, " lines long"))
    }
    text <- paste0(file$X1, collapse = "\n")
  }
  # Extract summary text

  summary_text <- extractSummaryText(text)
  # Extract all lines defining the family tree
  tree_lines <- unlist(stringr::str_extract_all(text, "\\{\\{familytree.*?\\}\\}"))
  tree_lines <- tree_lines[!stringr::str_detect(tree_lines, "start|end")] # Remove start/end markers
  tree_lines <- gsub("\\{\\{familytree(.*?)\\}\\}", "\\1", tree_lines) # Remove wrapping markup

  # Convert tree structure into a coordinate grid (preserves symbols!)
  tree_df <- parseTree(tree_lines)

  # Identify columns that start with "Y"
  cols_to_pivot <- grep("^Y", names(tree_df), value = TRUE)

  # Reshape from wide to long format
  tree_long <- makeLongTree(tree_df, cols_to_pivot)

  # Extract member definitions
  members_df <- matchMembers(text)
  members_df$id <- paste0("P", seq_len(nrow(members_df))) # Assign unique person IDs

  # Merge names into the tree structure (keeping all symbols!)
  tree_long <- merge(tree_long, members_df, by.x = "Value", by.y = "identifier", all.x = TRUE)

  tree_long$DisplayName <- ifelse(!is.na(tree_long$name), tree_long$name, tree_long$Value) # Use name if available

  # parse relationships and infer them

  relationships_df <- parseRelationships(tree_long)

  # relationships_df <- processParents(tree_long, datasource = "wiki")



  # Return structured table of the family tree (symbols included)
  list(
    summary = summary_text,
    members = members_df,
    structure = tree_long,
    relationships = relationships_df
  )
}

#' Make Long Tree
#' @param tree_df A data frame containing the tree structure.
#' @param cols_to_pivot A character vector of column names to pivot.
#' @return A long data frame containing the tree structure.
#' @keywords internal
makeLongTree <- function(tree_df, cols_to_pivot) {
  tree_long <- stats::reshape(tree_df,
    varying = cols_to_pivot,
    v.names = "Value",
    timevar = "Column",
    times = cols_to_pivot,
    idvar = setdiff(names(tree_df), cols_to_pivot),
    direction = "long"
  )

  tree_long <- tree_long[!is.na(tree_long$Value), ]
  tree_long$Value <- stringr::str_trim(tree_long$Value)
  tree_long$Column <- as.numeric(gsub("^Y", "", tree_long$Column))
  return(tree_long)
}

#' Match Members
#' @inheritParams readWikifamilytree
#' @return A data frame containing information about the members of the family tree.
#' @keywords internal

matchMembers <- function(text) {
  member_matches <- stringr::str_extract_all(text, "\\|\\s*([A-Za-z0-9]+)\\s*=\\s*([^|}]*)")[[1]]
  member_matches <- gsub("\\[|\\]|'''", "", member_matches) # Remove formatting

  members_df <- data.frame(
    identifier = stringr::str_trim(stringr::str_extract(member_matches, "^[^=]+")),
    name = stringr::str_trim(stringr::str_extract(member_matches, "(?<=\\=).*")),
    stringsAsFactors = FALSE
  )

  # Remove leading pipes (`|`) from identifiers for consistency
  members_df$identifier <- gsub("^\\|\\s*", "", members_df$identifier)

  # remove summary row
  members_df <- members_df[members_df$identifier != "summary", ]

  return(members_df)
}

#' Extract Summary Text
#' @inheritParams readWikifamilytree
#' @return A character string containing the summary text.
#' @keywords internal
#' @export

extractSummaryText <- function(text) {
  summary_match <- stringr::str_match(text, "\\{\\{familytree/start \\|summary=(.*?)\\}\\}")
  summary_text <- ifelse(!is.na(summary_match[, 2]), summary_match[, 2], NA)
  return(summary_text)
}

#' Parse Tree
#' @param tree_lines A character vector containing the lines of the tree structure.
#' @return A data frame containing the tree structure.
#' @keywords internal
#' @export

parseTree <- function(tree_lines) {
  tree_matrix <- base::strsplit(tree_lines, "\\|") # Split each row into columns
  max_cols <- max(sapply(tree_matrix, length)) # Find the max column count

  # Convert to a data frame (ensures correct structure)
  tree_df <- do.call(rbind, lapply(tree_matrix, function(row) {
    length(row) <- max_cols # Ensure uniform column length
    return(row)
  }))

  tree_df <- as.data.frame(tree_df, stringsAsFactors = FALSE)
  colnames(tree_df) <- paste0("Y", seq_len(ncol(tree_df))) # Assign column names
  tree_df$Row <- seq_len(nrow(tree_df)) # Assign row numbers
  return(tree_df)
}

#' infer relationship from tree template
#'
#' @param tree_long A data frame containing the tree structure in long format.
#' @return A data frame containing the relationships between family members.
#' @keywords internal
#'
parseRelationships <- function(tree_long) {
  relationships <- data.frame(
    id = tree_long$id,
    momID = NA_character_,
    dadID = NA_character_,
    spouseID = NA_character_,
    stringsAsFactors = FALSE
  )

  # Loop through rows to find connections
  for (i in seq_len(nrow(tree_long))) {
    row <- tree_long[i, ]

    # **Parent-Child Detection**
    if (row$Value == "y") {
      parent <- tree_long$Value[tree_long$Row == row$Row - 1 & tree_long$Column == row$Column]
      child <- tree_long$Value[tree_long$Row == row$Row + 1 & tree_long$Column == row$Column]

      if (length(parent) == 0) parent <- NA
      if (length(child) == 0) child <- NA
      # Assign mom/dad IDs based on tree structure
      if (!is.na(parent) && !is.na(child)) {
        relationships$momID[relationships$id == child] <- parent
        relationships$dadID[relationships$id == child] <- parent # Assuming one parent detected for now
      }
    }

    # **Spouse Detection**
    if (row$Value == "+") {
      spouse1 <- tree_long$Value[tree_long$Row == row$Row & tree_long$Column == row$Column - 1]
      spouse2 <- tree_long$Value[tree_long$Row == row$Row & tree_long$Column == row$Column + 1]

      if (!is.na(spouse1) && !is.na(spouse2)) {
        relationships$spouseID[relationships$id == spouse1] <- spouse2
        relationships$spouseID[relationships$id == spouse2] <- spouse1
      }
    }
  }

  return(relationships)
}
