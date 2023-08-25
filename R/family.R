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
ped2fam <- function(ped, personID='ID', momID='momID', dadID='dadID', famID='famID'){
    if(!inherits(ped, 'data.frame')) stop("ped should be a data.frame or inherit to a data.frame")
    if(!all(c() %in% names(ped))) stop("'personID', 'momID', and 'dadID' were not all found in your pedigree.\nMake sure you have the variable names correct.")

    # TODO call ped2graph
    nodes <- unique(
        na.omit(c(ped[[personID]], ped[[momID]], ped[[dadID]]))
    )
    edges <- rbind(
        as.matrix(ped[,c(personID, momID)]),
        as.matrix(ped[,c(personID, dadID)]))
    edges <- edges[complete.cases(edges),]

    # Make graph
    pg <- igraph::graph_from_data_frame(d=edges, directed=FALSE, vertices=nodes) # directed = TRUE looks better?

    # Find weakly connected components of graph
    wcc <- igraph::components(pg)

    fam <- data.frame(V1=as.numeric(names(wcc$membership)), V2=wcc$membership)
    names(fam) <- c(personID, famID)
    ped2 <- merge(fam, ped, by='ID', all.x=FALSE, all.y=TRUE)

    return(ped2)    
}

# take a pedigree and return a graph
ped2graph <- function(){return(0)}

