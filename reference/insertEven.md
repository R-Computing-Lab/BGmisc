# evenInsert A function to insert m elements evenly into a length n vector.

evenInsert A function to insert m elements evenly into a length n
vector.

## Usage

``` r
insertEven(m, n, verbose = FALSE)

evenInsert(m, n, verbose = FALSE)
```

## Arguments

- m:

  A numeric vector of length less than or equal to n. The elements to be
  inserted.

- n:

  A numeric vector. The vector into which the elements of m will be
  inserted.

- verbose:

  logical If TRUE, prints additional information. Default is FALSE.

## Value

Returns a numeric vector with the elements of m evenly inserted into n.

## Details

The function takes two vectors, m and n, and inserts the elements of m
evenly into n. If the length of m is greater than the length of n, the
vectors are swapped, and the insertion proceeds. The resulting vector is
a combination of m and n, with the elements of m evenly distributed
within n.

## See also

[`SimPed`](https://r-computing-lab.github.io/BGmisc/reference/simulatePedigree.md)
for the main function that uses this supporting function.
