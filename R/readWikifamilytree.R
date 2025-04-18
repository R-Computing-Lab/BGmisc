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

traced <-  traceTreePaths(tree_long, deduplicate = FALSE)

  # Initialize relationships data frame
  relationships <- data.frame(
    id = tree_long$id[!is.na(tree_long$id)],
    momID = NA_character_,
    dadID = NA_character_,
    parent_1 = NA_character_,
    parent_2 = NA_character_,
    spouseID = NA_character_,
    stringsAsFactors = FALSE
  )

  traced <- traced[!is.na(traced$from_id) & !is.na(traced$to_id), ]


 # > traced
#  from_id to_id path_length   intermediates intermediate_values
#        A     B           2             1_2                   +
#          A     C           5 1_2;2_2;3_2;4_2                +|y|
#          B     C           5 1_2;2_2;3_2;4_2                +|y|
  # Fill in relationships based on the tree structure
  traced$relationship <- NA_character_


  # if intermediate values is "+", the relationship is spouse


  traced$relationship[traced$intermediate_values=="+"] <- "spouse"

  #if intermediate value contains "y", the relationship is parent-child

  traced$relationship[traced$intermediate_values=="+|y|"] <- "parent-child"
  traced$relationship[traced$intermediate_values=="|y|+"] <- "child-parent"



  return(relationships)
}



#' Trace paths between individuals in a family tree grid
#'
#' @param tree_long A data.frame with columns: Row, Column, Value, id
#' @param deduplicate Logical, if TRUE, will remove duplicate paths
#' @return A data.frame with columns: from_id, to_id, direction, path_length, intermediates
#' @export
traceTreePaths <- function(tree_long, deduplicate = TRUE) {
  # Keep only relevant cells (people and path symbols)
  path_symbols <- c("|", "-", "+", "v", "^", "y", ",", ".", "`", "!")
  tree_long$Value <- gsub("\\s+", "", tree_long$Value) # Remove whitespace
  active_cells <- tree_long[!is.na(tree_long$Value) &
                              (tree_long$Value %in% path_symbols | !is.na(tree_long$id)), ]

  active_cells$key <- paste(active_cells$Row, active_cells$Column, sep = "_")


  edges <- do.call(rbind, lapply(seq_len(nrow(active_cells)), function(i) {
    from_key <- active_cells$key[i]
    to_keys <- findNeighbors(active_cells[i, ],
                             active_keys=active_cells$key)
    if (length(to_keys) > 0) {
      data.frame(from = from_key, to = to_keys, stringsAsFactors = FALSE)
    }
  }))

  # Create graph
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)

  # Map keys to IDs
  person_nodes <- active_cells[!is.na(active_cells$id), c("key", "id")]
  id_map <- setNames(person_nodes$id, person_nodes$key)

  # Find all pairs of people and trace paths
  person_keys <- names(id_map)
  result <- data.frame()

  for (i in seq_along(person_keys)) {
    for (j in seq_along(person_keys)) {
      if (i == j) next
      from_key <- person_keys[i]
      to_key <- person_keys[j]

      sp <- suppressWarnings(igraph::shortest_paths(g, from_key, to_key, output = "vpath")$vpath[[1]])
      if (length(sp) > 1) {
        intermediate <- setdiff(names(sp), c(from_key, to_key))
        # Extract values at those intermediate keys
        intermediate_values <- sapply(intermediate, function(k) {
          cell <- active_cells[active_cells$key == k, ]
          if (nrow(cell) > 0) cell$Value else NA
        })
        result <- rbind(result, data.frame(
          from_id = id_map[[from_key]],
          to_id = id_map[[to_key]],
          path_length = length(sp) - 1,
          intermediates = paste(intermediate, collapse = ";"),
          intermediate_values = paste(intermediate_values, collapse = ""),
          stringsAsFactors = FALSE
        ))} else {
          # If no path found, add a row with NA values
          result <- rbind(result, data.frame(
            from_id = id_map[[from_key]],
            to_id = id_map[[to_key]],
            path_length = NA,
            intermediates = NA,
            intermediate_values = NA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

  if(deduplicate==TRUE){
    # Deduplicate pairs
    result <- deduplicatePairs(result)
  }

  return(result)
}

#' Build adjacency list (4-way neighbors)
#'
#' @param cell A data frame with columns Row and Column
#' @return A character vector of neighboring cell keys
#' @keywords internal

findNeighbors <- function(cell,active_keys) {
  offsets <- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))  # down, up, right, left
  out <- character()
  for (offset in offsets) {
    r2 <- cell$Row + offset[1]
    c2 <- cell$Column + offset[2]
    key2 <- paste(r2, c2, sep = "_")
    if (key2 %in% active_keys) {
      out <- c(out, key2)
    }
  }
  return(out)
}

#' Deduplicate pairs of IDs in a data frame
#'
#' @param df A data frame with columns from_id and to_id
#' @return A data frame with unique pairs of IDs
#' @export
deduplicatePairs <- function(df) {
  # Create a new column with sorted pairs
  df$pair <- apply(df[, c("from_id", "to_id")], 1, function(x) paste(sort(x), collapse = "_"))

  # Remove duplicates based on the pair column
  df_dedup <- df[!duplicated(df$pair), ]

  # Drop the pair column
  df_dedup$pair <- NULL

  return(df_dedup)
}
