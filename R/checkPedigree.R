#' Validate Pedigree Network Structure
#'
#' Checks for structural issues in pedigree networks, including:
#' - Individuals with more than two parents.
#' - Presence of cyclic parent-child relationships.
#'
#' @param ped Dataframe representing the pedigree.
#' @param personID Character. Column name for individual IDs.
#' @param momID Character. Column name for maternal IDs.
#' @param dadID Character. Column name for paternal IDs.
#' @param verbose Logical. If TRUE, print informative messages.
#'
#' @return List containing detailed validation results.
#' @examples
#' \dontrun{
#' results <- checkPedigreeNetwork(ped, personID = "ID", momID = "momID", dadID = "dadID", verbose = TRUE)
#' }
#' @export
checkPedigreeNetwork <- function(ped, personID = "ID", momID = "momID", dadID = "dadID", verbose = FALSE) {
  # Create directed edges from parent to child relationships
  ped_edges <- rbind(
    data.frame(from = ped[[momID]], to = ped[[personID]]),
    data.frame(from = ped[[dadID]], to = ped[[personID]])
  )
  ped_edges <- ped_edges[!is.na(ped_edges$from) & !is.na(ped_edges$to), ]

  ped_graph <- igraph::graph_from_data_frame(ped_edges, directed = TRUE)

  results <- list()

  ## Check for individuals with more than two parents
  indegrees <- igraph::degree(ped_graph, mode = "in")
  ids_excess_parents <- names(indegrees[indegrees > 2])

  results$individuals_with_excess_parents <- ids_excess_parents

  if (verbose) {
    if (length(ids_excess_parents) > 0) {
      message("Individuals with more than two parents detected: ", paste(ids_excess_parents, collapse = ", "))
    } else {
      message("No individuals with more than two parents detected.")
    }
  }

  ## Check for duplicate edges

  duplicate_edges_idx <- igraph::which_multiple(ped_graph)
  duplicate_edges <- igraph::as_edgelist(ped_graph)[duplicate_edges_idx, , drop = FALSE]

  results$duplicate_edges <- duplicate_edges

  if (verbose) {
    if (nrow(duplicate_edges) > 0) {
      message("Duplicate edges detected:")
      print(duplicate_edges)
    } else {
      message("No duplicate edges detected.")
    }
  }
  ## Check for cyclic relationships
  is_acyclic <- igraph::is_dag(ped_graph)
  results$is_acyclic <- is_acyclic

  if (!is_acyclic) {
    cyclic_edges <- igraph::feedback_arc_set(ped_graph)
    cyclic_relationships <- igraph::as_edgelist(ped_graph)[cyclic_edges, ]
    results$cyclic_relationships <- cyclic_relationships
    if (verbose) {
      message("Cyclic relationships detected:")
      print(cyclic_relationships)
    }
  } else {
    results$cyclic_relationships <- NULL
    if (verbose) message("No cyclic relationships detected.")
  }

  return(results)
}
