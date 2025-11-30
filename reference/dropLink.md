# dropLink

A function to drop a person from his/her parents in the simulated
pedigree `data.frame`. The person can be dropped by specifying his/her
ID or by specifying the generation which the randomly to-be-dropped
person is in. The function can separate one pedigree into two pedigrees.
Separating into small pieces should be done by running the function
multiple times. This is a supplementary function for `simulatePedigree`.

## Usage

``` r
dropLink(
  ped,
  ID_drop = NA_integer_,
  gen_drop = 2,
  sex_drop = NA_character_,
  n_drop = 1
)
```

## Arguments

- ped:

  a pedigree simulated from simulatePedigree function or the same format

- ID_drop:

  the ID of the person to be dropped from his/her parents.

- gen_drop:

  the generation in which the randomly dropped person is. Will work if
  \`ID_drop\` is not specified.

- sex_drop:

  the biological sex of the randomly dropped person.

- n_drop:

  the number of times the mutation happens.

## Value

a pedigree with the dropped person's \`dadID\` and \`momID\` set to NA.
