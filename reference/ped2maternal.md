# Add a maternal line ID variable to a pedigree

Add a maternal line ID variable to a pedigree

## Usage

``` r
ped2maternal(
  ped,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  matID = "matID",
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

- matID:

  Character. Maternal line ID variable to be created and added to the
  pedigree

- ...:

  additional arguments to be passed to
  [`ped2com`](https://r-computing-lab.github.io/BGmisc/reference/ped2com.md)

## Details

Under various scenarios it is useful to know which people in a pedigree
belong to the same maternal lines. This function first turns a pedigree
into a graph where adjacency is defined by mother-child relationships.
Subsequently, the weakly connected components algorithm finds all the
separate maternal lines and gives them an ID variable.

## See also

\[ped2fam()\] for creating extended family IDs, and \[ped2paternal()\]
for creating paternal line IDs
