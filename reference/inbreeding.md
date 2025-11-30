# Artificial pedigree data on eight families with inbreeding

A dataset created purely from imagination that includes several types of
inbreeding. Different kinds of inbreeding occur in each extended family.

The types of inbreeding are as follows:

- Extended Family 1: Sister wives - Children with the same father and
  different mothers who are sisters.

- Extended Family 2: Full siblings have children.

- Extended Family 3: Half siblings have children.

- Extended Family 4: First cousins have children.

- Extended Family 5: Father has child with his daughter.

- Extended Family 6: Half sister wives - Children with the same father
  and different mothers who are half sisters.

- Extended Family 7: Uncle-niece and Aunt-nephew have children.

- Extended Family 8: A father-son pairs has children with a
  corresponding mother-daughter pair.

Although not all of the above structures are technically inbreeding,
they aim to test pedigree diagramming and path tracing algorithms. This
dataset is not intended to represent any real individuals or families.

The variables are as follows:

- `ID`: Person identification variable

- `sex`: Sex of the ID: 1 is female; 0 is male

- `dadID`: ID of the father

- `momID`: ID of the mother

- `FamID`: ID of the extended family

- `Gen`: Generation of the person

- `proband`: Always FALSE

## Usage

``` r
data(inbreeding)
```

## Format

A data frame (and ped object) with 134 rows and 7 variables
