# Turn a pedigree into a graph

Turn a pedigree into a graph

## Usage

``` r
ped2graph(
  ped,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  directed = TRUE,
  adjacent = c("parents", "mothers", "fathers"),
  ...
)
```

## Arguments

- ped:

  a pedigree dataset. Needs ID, momID, and dadID columns

- personID:

  character. Name of the column in ped for the person ID variable

- momID:

  character. Name of the column in ped for the mother ID variable

- dadID:

  character. Name of the column in ped for the father ID variable

- directed:

  Logical scalar. Default is TRUE. Indicates whether or not to create a
  directed graph.

- adjacent:

  Character. Relationship that defines adjacency in the graph: parents,
  mothers, or fathers

- ...:

  additional arguments to be passed to
  [`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)

## Value

A graph

## Details

The general idea of this function is to represent a pedigree as a graph
using the igraph package.

Once in graph form, several common pedigree tasks become much simpler.

The `adjacent` argument allows for different kinds of graph structures.
When using `parents` for adjacency, the graph shows all parent-child
relationships. When using `mother` for adjacency, the graph only shows
mother-child relationships. Similarly when using `father` for adjacency,
only father-child relationships appear in the graph. Construct extended
families from the parent graph, maternal lines from the mothers graph,
and paternal lines from the fathers graph.
