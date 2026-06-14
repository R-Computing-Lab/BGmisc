#' This is a convenience wrapper around [addParentalChain()] with
#' `component = "dadID"`.
#'
#' @param ped A pedigree data frame.
#' @param personID Character string giving the name of the column containing
#'   individual IDs.
#' @param dadID Character string giving the name of the column containing
#'   paternal IDs.
#' @param momID Character string giving the name of the column containing
#'   maternal IDs.
#' @param chain_col Character string giving the name of the output list-column
#'   that will contain the ordered paternal ancestor chain for each individual.
#' @param chain_string_col Character string giving the name of the output
#'   character column that will contain the collapsed paternal ancestor chain.
#' @param collapse Character string used to collapse ancestor IDs into
#'   `chain_string_col`.
#'  @param traversal_direction Character giving the mode of transversion: defaults to "in"
#'
#' @return A data frame with two added columns:
#'   \describe{
#'     \item{`chain_col`}{A list-column containing the ordered paternal ancestor
#'     chain for each individual.}
#'     \item{`chain_string_col`}{A character column containing the collapsed
#'     paternal ancestor chain, or `NA_character_` when no paternal ancestors are
#'     found.}
#'   }
#'
#' @export
addPaternalChain <- function(
    ped,
    personID = "personID",
    dadID = "dadID",
    momID = "momID",
    chain_col = "dadID_chain",
    chain_string_col = "dadID_chain_string",
    collapse = "|",
    traversal_direction = "in"
) {
addParentalChain(
    ped = ped,
    personID = personID,
    dadID = dadID,
    momID = momID,
    chain_col = chain_col,
    chain_string_col = chain_string_col,
    collapse = collapse,
    component = "dadID",
    traversal_direction = traversal_direction
)
}

#' Add maternal ancestor chains to a pedigree
#'
#' Adds an ordered maternal ancestor chain for each individual in a pedigree.
#' The chain follows only mother-to-mother links, so the resulting chain is:
#' mother, maternal grandmother, maternal great-grandmother, and so on.
#'
#' This is a convenience wrapper around [addParentalChain()] with
#' `component = "momID"`.
#'
#' @param ped A pedigree data frame.
#' @param personID Character string giving the name of the column containing
#'   individual IDs.
#' @param dadID Character string giving the name of the column containing
#'   paternal IDs.
#' @param momID Character string giving the name of the column containing
#'   maternal IDs.
#' @param chain_col Character string giving the name of the output list-column
#'   that will contain the ordered maternal ancestor chain for each individual.
#' @param chain_string_col Character string giving the name of the output
#'   character column that will contain the collapsed maternal ancestor chain.
#' @param collapse Character string used to collapse ancestor IDs into
#'   `chain_string_col`.
#'
#' @return A data frame with two added columns:
#'   \describe{
#'     \item{`chain_col`}{A list-column containing the ordered maternal ancestor
#'     chain for each individual.}
#'     \item{`chain_string_col`}{A character column containing the collapsed
#'     maternal ancestor chain, or `NA_character_` when no maternal ancestors are
#'     found.}
#'   }
#'
#' @export

addMaternalChain <- function(
    ped,
    personID = "personID",
    dadID = "dadID",
    momID = "momID",
    chain_col = "momID_chain",
    chain_string_col = "momID_chain_string",
    collapse = "|",
    traversal_direction = "in"
) {
addParentalChain(
    ped = ped,
    personID = personID,
    dadID = dadID,
    momID = momID,
    chain_col = chain_col,
    chain_string_col = chain_string_col,
    collapse = collapse,
    component = "momID",
    traversal_direction = traversal_direction
)
}
#' Add unilineal parental ancestor chains to a pedigree
#'
#' Adds an ordered unilineal parental ancestor chain for each individual in a
#' pedigree. The chain can follow either paternal links only or maternal links
#' only.
#'
#' For `component = "dadID"`, the chain follows:
#' father, paternal grandfather, paternal great-grandfather, and so on.
#'
#' For `component = "momID"`, the chain follows:
#' mother, maternal grandmother, maternal great-grandmother, and so on.
#'
#' The function constructs a parent-specific version of the pedigree, converts it
#' to a graph using [ped2graph()], identifies reachable ancestors for each
#' individual, orders those ancestors by graph distance from the focal
#' individual, and adds both a list-column representation and a collapsed string
#' representation to the original pedigree.
#'
#' @param ped A pedigree data frame.
#' @param personID Character string giving the name of the column containing
#'   individual IDs.
#' @param dadID Character string giving the name of the column containing
#'   paternal IDs.
#' @param momID Character string giving the name of the column containing
#'   maternal IDs.
#' @param chain_col Character string giving the name of the output list-column
#'   that will contain the ordered parental ancestor chain for each individual.
#' @param chain_string_col Character string giving the name of the output
#'   character column that will contain the collapsed parental ancestor chain.
#' @param collapse Character string used to collapse ancestor IDs into
#'   `chain_string_col`.
#' @param component Character string specifying which parental component to
#'   follow. Must be either `"dadID"` for paternal chains or `"momID"` for
#'   maternal chains.
#'
#' @return A data frame with two added columns:
#'   \describe{
#'     \item{`chain_col`}{A list-column containing the ordered unilineal parental
#'     ancestor chain for each individual.}
#'     \item{`chain_string_col`}{A character column containing the collapsed
#'     ancestor chain, or `NA_character_` when no ancestors are found in the
#'     selected component.}
#'   }
#'
#' @details
#' All pedigree IDs used to construct the parent-specific graph are coerced to
#' character. Individuals that are not represented as graph vertices receive an
#' empty chain in `chain_col` and `NA_character_` in `chain_string_col`.
#'
#' The ordering of each chain is based on graph distance from the focal
#' individual, where distance 1 is the selected parent, distance 2 is the
#' selected parent's same-component parent, and so forth.
#'
#' @importFrom igraph V subcomponent distances
#'
#' @export

addParentalChain <- function(
    ped,
    personID = "personID",
    dadID = "dadID",
    momID = "momID",
    chain_col = "chain",
    chain_string_col = "chain_string",
    collapse = "|",
    component = c("dadID",  "momID"),
    traversal_direction = "in"
) {

  # Build a paternal-only version of the pedigree.
  # This removes maternal edges so the resulting graph only represents:
  # person -> father -> father's father -> father's father's father.

  component <- match.arg(component)

  if (component == "momID") {
    ##############
    # Maternal-only pedigree.
    ##############
    parental_ped <-
      data.frame(
        personID = as.character(ped[[personID]]),
        momID = as.character(ped[[momID]]),
        dadID = NA_character_,
      stringsAsFactors = FALSE
      )
  } else if (component == "dadID") {
    ##############
    # Paternal-only pedigree.
    ##############
  parental_ped <- data.frame(
      personID = as.character(ped[[personID]]),
      momID = NA_character_,
      dadID = as.character(ped[[dadID]]),
    stringsAsFactors = FALSE
    )
} else {
 stop("unknown id supplied")
}
  # Use BGmisc infrastructure to convert the paternal-only pedigree into a graph.
  parental_graph <- ped2graph(
    parental_ped,
    personID = "personID",
    momID = "momID",
    dadID = "dadID"
  )

  # For one person, recover the ordered paternal chain from the network.
  get_ordered_parental_chain <- function(id) {

    id_chr <- as.character(id)

    # If the person is not represented as a graph vertex, return no chain.
    if (!id_chr %in% igraph::V(parental_graph)$name) {
      return(character(0))
    }

    # Find all nodes reachable from this person by following paternal edges.
    reachable_ids <- igraph::subcomponent(
      graph = parental_graph,
      v = id_chr,
      mode = traversal_direction
    ) |>
      names()

    # Remove the person themselves from their paternal ancestor list.
    reachable_ids <- setdiff(reachable_ids, id_chr)

    # If no paternal ancestors are reachable, return no chain.
    if (length(reachable_ids) == 0) {
      return(character(0))
    }

    # Order ancestors by graph distance.
    # Distance 1 = father.
    # Distance 2 = paternal grandfather.
    # Distance 3 = paternal great-grandfather.
    ancestor_distances <- igraph::distances(
      graph = parental_graph,
      v = id_chr,
      to = reachable_ids,
      mode = traversal_direction
    )[1, ]

    reachable_ids[order(ancestor_distances)]
  }

  # Add the ordered paternal chain as a list-column.
  ped[[chain_col]] <- lapply(
    ped[[personID]],
    get_ordered_parental_chain
  )

  # Add a readable string version for inspection.
  ped[[chain_string_col]] <- vapply(
    ped[[chain_col]],
    function(chain) {
      if (length(chain) == 0) {
        return(NA_character_)
      }

      paste(chain, collapse = collapse)
    },
    character(1)
  )

  ped
}

#' Add a paternal-line descendant flag to a pedigree
#'
#' Adds a logical flag indicating whether a specified anchor individual appears
#' anywhere in each person's ordered paternal ancestor chain.
#'
#' This is a convenience wrapper around [addParentalLineFlag()] with
#' `component = "dadID"`.
#'
#' @param ped A pedigree data frame containing a paternal chain list-column.
#' @param anchor_id ID of the anchor individual to search for within each
#'   person's paternal chain.
#' @param flag_col Character string giving the name of the logical output column
#'   to add to `ped`.
#' @param chain_col Character string giving the name of the list-column
#'   containing ordered paternal ancestor chains.
#'
#' @return A data frame with `flag_col` added. The flag is `TRUE` when
#'   `anchor_id` appears in the individual's paternal chain and `FALSE`
#'   otherwise.
#'
#' @export
addPaternalLineFlag <- function(
    ped,
    anchor_id,
    flag_col,
    chain_col = "dadID_chain"
) {
  addParentalLineFlag(
    ped = ped,
    anchor_id = anchor_id,
    flag_col = flag_col,
    chain_col = chain_col,
    component = "dadID"
  )
}

#' Add a maternal-line descendant flag to a pedigree
#'
#' Adds a logical flag indicating whether a specified anchor individual appears
#' anywhere in each person's ordered maternal ancestor chain.
#'
#' This is a convenience wrapper around [addParentalLineFlag()] with
#' `component = "momID"`.
#'
#' @param ped A pedigree data frame containing a maternal chain list-column.
#' @param anchor_id ID of the anchor individual to search for within each
#'   person's maternal chain.
#' @param flag_col Character string giving the name of the logical output column
#'   to add to `ped`.
#' @param chain_col Character string giving the name of the list-column
#'   containing ordered maternal ancestor chains.
#'
#' @return A data frame with `flag_col` added. The flag is `TRUE` when
#'   `anchor_id` appears in the individual's maternal chain and `FALSE`
#'   otherwise.
#'
#' @export
addMaternalLineFlag <- function(
    ped,
    anchor_id,
    flag_col,
    chain_col = "momID_chain"
) {
  addParentalLineFlag(
    ped = ped,
    anchor_id = anchor_id,
    flag_col = flag_col,
    chain_col = chain_col,
    component = "momID"
  )
}

#' Add a unilineal parental-line descendant flag to a pedigree
#'
#' Adds a logical flag indicating whether a specified anchor individual appears
#' anywhere in each person's ordered unilineal parental ancestor chain.
#'
#' For `component = "dadID"`, the function searches the paternal chain.
#' For `component = "momID"`, the function searches the maternal chain.
#'
#' @param ped A pedigree data frame containing a parental chain list-column.
#' @param anchor_id ID of the anchor individual to search for within each
#'   person's parental chain.
#' @param flag_col Character string giving the name of the logical output column
#'   to add to `ped`.
#' @param chain_col Character string giving the name of the list-column
#'   containing ordered parental ancestor chains.
#' @param component Character string specifying which parental component the
#'   chain represents. Must be either `"dadID"` for paternal chains or `"momID"`
#'   for maternal chains.
#'
#' @return A data frame with `flag_col` added. The flag is `TRUE` when
#'   `anchor_id` appears in the individual's selected parental chain and `FALSE`
#'   otherwise.
#'
#' @details
#' The anchor ID is coerced to character before comparison because the chain
#' columns produced by [addParentalChain()] store graph vertex names as character
#' values.
#'
#' This function assumes that `chain_col` is a list-column in which each element
#' is a character vector of ancestor IDs. Empty chains should be represented as
#' `character(0)`.
#'
#' @export
addParentalLineFlag <- function(
    ped,
    anchor_id,
    flag_col,
    chain_col,
    component = c("dadID", "momID")
) {

  component <- match.arg(component)

  # Convert the anchor ID to character because the parental chain stores graph
  # vertex names as character values.
  anchor_id_chr <- as.character(anchor_id)

  # Create a named logical flag indicating whether the anchor appears anywhere
  # in each person's ordered parental chain.
  ped[[flag_col]] <- vapply(
    ped[[chain_col]],
    function(chain) {
      anchor_id_chr %in% chain
    },
    logical(1)
  )

  ped
}
