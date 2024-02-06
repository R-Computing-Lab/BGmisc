#' Add an extended family ID variable to a pedigree
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @param personID character.  Name of the column in ped for the person ID variable
#' @param momID character.  Name of the column in ped for the mother ID variable
#' @param dadID character.  Name of the column in ped for the father ID variable
#' @param famID character.  Name of the column to be created in ped for the family ID variable
#' @details
#' The general idea of this function is to use person ID, mother ID, and father ID to
#' create an extended family ID such that everyone with the same family ID is in the
#' same (perhaps very extended) pedigree.  That is, a pair of people with the same family ID
#' have at least one traceable relation of any length to one another.
#'
#' This function works by turning the pedigree into a mathematical graph using the igraph
#' package.  Once in graph form, the function uses weakly connected components to search
#' for all possible relationship paths that could connect anyone in the data to anyone
#' else in the data.
#'
#' @returns
#' A pedigree dataset with one additional column for the newly created extended family ID
#'
#' @export
#'
ped2fam <- function(ped, personID = "ID", momID = "momID", dadID = "dadID", famID = "famID") {
  # Call to wrapper function
  .ped2id(ped = ped, personID = personID, momID = momID, dadID = dadID, famID = famID, type = "parents")
}

.ped2id <- function(ped,
                    personID = "ID", momID = "momID", dadID = "dadID",
                    famID = "famID", type) {
  # Turn pedigree into family
  pg <- ped2graph(ped = ped, personID = personID, momID = momID, dadID = dadID, adjacent = type)

  # Find weakly connected components of graph
  wcc <- igraph::components(pg)

  fam <- data.frame(
    V1 = as.numeric(names(wcc$membership)),
    V2 = wcc$membership
  )
  names(fam) <- c(personID, famID)
  ped2 <- merge(fam, ped,
    by = personID, all.x = FALSE, all.y = TRUE
  )

  return(ped2)
}


#' Turn a pedigree into a graph
#' @param ped a pedigree dataset.  Needs ID, momID, and dadID columns
#' @inheritParams ped2fam
#' @param directed Logical scalar. Default is TRUE. Indicates whether or not to create a directed graph.
#' @param adjacent Character.  Relationship that defines adjacency in the graph: parents, mothers, or fathers
#' @details
#' The general idea of this function is to represent a pedigree as a graph using the igraph package.
#'
#' Once in graph form, several common pedigree tasks become much simpler.
#'
#' The \code{adjacent} argument allows for different kinds of graph structures.
#' When using \code{parents} for adjacency, the graph shows all parent-child relationships.
#' When using \code{mother} for adjacency, the graph only shows mother-child relationships.
#' Similarly when using \code{father} for adjacency, only father-child relationships appear in the graph.
#' Extended families can be constructed from the parent graph, maternal lines from the mothers graph, and paternal lines from the fathers graph.
#'
#' @returns
#' A graph
#'
#' @export
#'
ped2graph <- function(ped,
                      personID = "ID",
                      momID = "momID",
                      dadID = "dadID",
                      directed = TRUE,
                      adjacent = c("parents", "mothers", "fathers")) {
  # Check ped/data.fram
  if (!inherits(ped, "data.frame")) stop("ped should be a data.frame or inherit to a data.frame")
  # Handle adjacent argument
  adjacent <- match.arg(tolower(adjacent)[1],
    choices = c(
      "parents",
      "mothers",
      "fathers"
    )
  )
  # Check the needed IDs are in the data
  if (adjacent == "parents") {
    needIds <- c(personID, momID, dadID)
  } else if (adjacent == "mothers") {
    needIds <- c(personID, momID)
  } else if (adjacent == "fathers") {
    needIds <- c(personID, dadID)
  }
  if (!all(c(needIds) %in% names(ped))) {
    msg <- paste0(
      "The following ID variables are needed but were not found:\n",
      paste(needIds[!(c(needIds) %in% names(ped))], collapse = ", "),
      "\n",
      "Make sure you have the variable names correct."
    )
    stop(msg)
  }

  # Create nodes and edges
  if (adjacent == "parents") {
    nodes <- unique(
      stats::na.omit(
        as.character(c(ped[[personID]], ped[[momID]], ped[[dadID]]))
      )
    )
    edges <- rbind(
      as.matrix(sapply(ped[, c(personID, momID)], as.character)),
      as.matrix(sapply(ped[, c(personID, dadID)], as.character))
    )
  } else if (adjacent == "mothers") {
    nodes <- unique(
      stats::na.omit(
        as.character(c(ped[[personID]], ped[[momID]]))
      )
    )
    edges <- as.matrix(sapply(ped[, c(personID, momID)], as.character))
  } else if (adjacent == "fathers") {
    nodes <- unique(
      stats::na.omit(
        as.character(c(ped[[personID]], ped[[dadID]]))
      )
    )
    edges <- as.matrix(sapply(ped[, c(personID, dadID)], as.character))
  }
  edges <- edges[stats::complete.cases(edges), ]

  # Make graph
  pg <- igraph::graph_from_data_frame(
    d = edges,
    directed = directed, # directed = TRUE looks better
    vertices = nodes
  )

  return(pg)
}

#' Add a maternal line ID variable to a pedigree
#' @inheritParams ped2fam
#' @param matID Character.  Maternal line ID variable to be created and added to the pedigree
#' @details
#' Under various scenarios it is useful to know which people in a pedigree
#' belong to the same maternal lines.  This function first turns a pedigree
#' into a graph where adjacency is defined by mother-child relationships.
#' Subsequently, the weakly connected components algorithm finds all the
#' separate maternal lines and gives them an ID variable.
#' @seealso [ped2fam()] for creating extended family IDs, and [ped2paternal()]
#' for creating paternal line IDs
#' @export
#'
ped2maternal <- function(ped, personID = "ID", momID = "momID", dadID = "dadID", matID = "matID") {
  # Call to wrapper function
  .ped2id(ped = ped, personID = personID, momID = momID, dadID = dadID, famID = matID, type = "mothers")
}

#' Add a paternal line ID variable to a pedigree
#' @inheritParams ped2fam
#' @param patID Character.  Paternal line ID variable to be created and added to the pedigree
#' @details
#' Under various scenarios it is useful to know which people in a pedigree
#' belong to the same paternal lines.  This function first turns a pedigree
#' into a graph where adjacency is defined by father-child relationships.
#' Subsequently, the weakly connected components algorithm finds all the
#' separate paternal lines and gives them an ID variable.
#' @seealso [ped2fam()] for creating extended family IDs, and [ped2maternal()]
#' for creating maternal line IDs
#' @export
#'
ped2paternal <- function(ped, personID = "ID", momID = "momID", dadID = "dadID", patID = "patID") {
  # Call to wrapper function
  .ped2id(ped = ped, personID = personID, momID = momID, dadID = dadID, famID = patID, type = "fathers")
}
