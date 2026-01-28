# Mark and Assign children

This subfunction marks individuals in a generation as potential sons,
daughters, or parents based on their relationships and assigns unique
couple IDs. It processes the assignment of roles and relationships
within and between generations in a pedigree simulation.

## Usage

``` r
markPotentialChildren(
  df_Ngen,
  i,
  Ngen,
  sizeGens,
  CoupleF,
  code_male = "M",
  code_female = "F",
  beta = FALSE
)

markPotentialChildren_beta(
  df_Ngen,
  i,
  Ngen,
  sizeGens,
  CoupleF,
  code_male = "M",
  code_female = "F"
)
```

## Arguments

- df_Ngen:

  A data frame for the current generation being processed. It must
  include columns for individual IDs (\`id\`), spouse IDs (\`spID\`),
  sex (\`sex\`), and any previously assigned roles (\`ifparent\`,
  \`ifson\`, \`ifdau\`).

- i:

  Integer, the index of the current generation being processed.

- Ngen:

  Integer, the total number of generations in the simulation.

- sizeGens:

  Numeric vector, containing the size (number of individuals) of each
  generation.

- CoupleF:

  Integer scalar giving the number of distinct mating couples in the
  current generation \`i\`. This is typically computed upstream from the
  spouse assignments (e.g., as the number of unique non-missing spouse
  pairs in \`df_Ngen\`) and must satisfy \`0 \<= CoupleF \<=
  floor(sizeGens\[i\] / 2)\`.

- code_male:

  The value to use for males. Default is "M"

- code_female:

  The value to use for females. Default is "F"

- beta:

  logical. If TRUE, use the optimized version of the algorithm.

## Value

Modifies \`df_Ngen\` in place by updating or adding columns related to
individual roles (\`ifparent\`, \`ifson\`, \`ifdau\`) and couple IDs
(\`coupleId\`). The updated data frame is also returned for integration
into the larger pedigree data frame (\`df_Fam\`).
