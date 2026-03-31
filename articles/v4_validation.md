# Validating and Repairing Pedigree Data with BGmisc

## Introduction

Working with pedigree data often involves dealing with inconsistencies,
missing information, and errors. The `BGmisc` package provides tools to
identify and, where possible, repair these issues automatically. This
vignette demonstrates how to validate and clean pedigree data using
`BGmisc`’s validation functions.

## Identifying and Repairing ID Issues

The
[`checkIDs()`](https://r-computing-lab.github.io/BGmisc/reference/checkIDs.md)
function detects two types of common ID errors in pedigree data:

- Between-row duplication: When two or more individuals share the same
  ID
- Within-row duplication: When an individual’s parents’ IDs are
  incorrectly listed as their own ID

These problems are especially common when merging family records or
processing historical data. Let’s explore how they show up — and what
they imply for downstream structure.

### A Clean Dataset

We’ll begin with the Potter family dataset, cleaned and reformatted with
[`ped2fam()`](https://r-computing-lab.github.io/BGmisc/reference/ped2fam.md):

``` r
library(BGmisc)

# Load our example dataset
df <- ped2fam(potter, famID = "newFamID", personID = "personID")

# Check for ID issues
checkIDs(df, repair = FALSE)
#> $all_unique_ids
#> [1] TRUE
#> 
#> $total_non_unique_ids
#> [1] 0
#> 
#> $non_unique_ids
#> NULL
#> 
#> $total_own_father
#> [1] 0
#> 
#> $total_own_mother
#> [1] 0
#> 
#> $total_duplicated_parents
#> [1] 0
#> 
#> $total_within_row_duplicates
#> [1] 0
#> 
#> $within_row_duplicates
#> [1] FALSE
#> 
#> $is_own_father_ids
#> NULL
#> 
#> $is_own_mother_ids
#> NULL
#> 
#> $duplicated_parents_ids
#> NULL
```

There are no duplicated or self-referential IDs here. But things rarely
stay that simple.

#### What `checkIDs()` Reports

The
[`checkIDs()`](https://r-computing-lab.github.io/BGmisc/reference/checkIDs.md)
function checks for:

- Whether all IDs are unique (reported by `all_unique_ids`, which tells
  you if all IDs in the dataset are unique, and `total_non_unique_ids`,
  which gives you the count of non-unique IDs found)
- Cases where someone’s ID matches their parent’s ID (shown in
  `total_own_father` and `total_own_mother`, which count individuals
  whose father’s or mother’s ID matches their own ID)
- Total duplicated parent IDs (tracked by `total_duplicated_parents`,
  which counts individuals with duplicated parent IDs)
- Within-row duplicates (measured by `total_within_row_duplicates`
  showing the count and `within_row_duplicates` indicating their
  presence)

If you set `repair = TRUE`, the function will attempt to fix the issues
it finds. We’ll explore this later.

### A Tale of Two Duplicates

To understand how these tools work in practice, let’s create a dataset
with two common real-world problems. First, we’ll accidentally give
Vernon Dursley the same ID as his sister Marjorie (a common issue when
merging family records). Then, we’ll add a complete duplicate of Dudley
Dursley (as might happen during data entry).

``` r
# Create our problematic dataset
df_duplicates <- df
# Sibling ID conflict
df_duplicates$personID[df_duplicates$name == "Vernon Dursley"] <-
  df_duplicates$personID[df_duplicates$name == "Marjorie Dursley"]
# Duplicate entry
df_duplicates <- rbind(
  df_duplicates,
  df_duplicates[df_duplicates$name == "Dudley Dursley", ]
)
```

If we look at the data using standard tools, the problems aren’t
immediately obvious:

``` r
library(tidyverse)

summarizeFamilies(df_duplicates,
  famID = "newFamID",
  personID = "personID"
)$family_summary %>%
  glimpse()
#> Rows: 1
#> Columns: 27
#> $ newFamID        <dbl> 1
#> $ count           <int> 37
#> $ famID_mean      <dbl> 1
#> $ famID_median    <dbl> 1
#> $ famID_min       <dbl> 1
#> $ famID_max       <dbl> 1
#> $ famID_sd        <dbl> 0
#> $ gen_mean        <dbl> 1.756757
#> $ gen_median      <dbl> 2
#> $ gen_min         <dbl> 0
#> $ gen_max         <dbl> 3
#> $ gen_sd          <dbl> 1.038305
#> $ spouseID_mean   <dbl> 38.2
#> $ spouseID_median <dbl> 15
#> $ spouseID_min    <dbl> 1
#> $ spouseID_max    <dbl> 106
#> $ spouseID_sd     <dbl> 44.15118
#> $ sex_mean        <dbl> 0.5135135
#> $ sex_median      <dbl> 1
#> $ sex_min         <dbl> 0
#> $ sex_max         <dbl> 1
#> $ sex_sd          <dbl> 0.5067117
#> $ twinID_mean     <dbl> 12.5
#> $ twinID_median   <dbl> 12.5
#> $ twinID_min      <dbl> 12
#> $ twinID_max      <dbl> 13
#> $ twinID_sd       <dbl> 0.7071068
```

But
[`checkIDs()`](https://r-computing-lab.github.io/BGmisc/reference/checkIDs.md)
detects the problems clearly:

``` r
# Identify duplicates
result <- checkIDs(df_duplicates)
print(result)
#> $all_unique_ids
#> [1] FALSE
#> 
#> $total_non_unique_ids
#> [1] 4
#> 
#> $non_unique_ids
#> [1] 2 6
#> 
#> $total_own_father
#> [1] 0
#> 
#> $total_own_mother
#> [1] 0
#> 
#> $total_duplicated_parents
#> [1] 0
#> 
#> $total_within_row_duplicates
#> [1] 0
#> 
#> $within_row_duplicates
#> [1] FALSE
#> 
#> $is_own_father_ids
#> NULL
#> 
#> $is_own_mother_ids
#> NULL
#> 
#> $duplicated_parents_ids
#> NULL
```

As we can see from this output, there are 4 non-unique IDs in the
dataset, specifically 2, 6. Let’s take a peek at the duplicates:

``` r
# Let's examine the problematic entries
df_duplicates %>%
  filter(personID %in% result$non_unique_ids) %>%
  arrange(personID)
#>    personID newFamID famID             name first_name surname gen momID dadID
#> 1         2        1     1   Vernon Dursley     Vernon Dursley   1   101   102
#> 2         2        1     1 Marjorie Dursley   Marjorie Dursley   1   101   102
#> 6         6        1     1   Dudley Dursley     Dudley Dursley   2     3     1
#> 61        6        1     1   Dudley Dursley     Dudley Dursley   2     3     1
#>    spouseID sex twinID zygosity
#> 1         3   1     NA     <NA>
#> 2        NA   0     NA     <NA>
#> 6        NA   1     NA     <NA>
#> 61       NA   1     NA     <NA>
```

Yep, these are definitely the duplicates.

#### Repairing Between-Row Duplicates

Some ID issues can be fixed automatically. Let’s try the repair option:

``` r
df_repair <- checkIDs(df, repair = TRUE)

df_repair %>%
  filter(ID %in% result$non_unique_ids) %>%
  arrange(ID)
#>   ID newFamID famID             name first_name surname gen momID dadID spID
#> 1  2        1     1 Marjorie Dursley   Marjorie Dursley   1   101   102   NA
#> 2  6        1     1   Dudley Dursley     Dudley Dursley   2     3     1   NA
#>   sex twinID zygosity
#> 1   0     NA     <NA>
#> 2   1     NA     <NA>

result <- checkIDs(df_repair)

print(result)
#> $all_unique_ids
#> [1] TRUE
#> 
#> $total_non_unique_ids
#> [1] 0
#> 
#> $non_unique_ids
#> NULL
#> 
#> $total_own_father
#> [1] 0
#> 
#> $total_own_mother
#> [1] 0
#> 
#> $total_duplicated_parents
#> [1] 0
#> 
#> $total_within_row_duplicates
#> [1] 0
#> 
#> $within_row_duplicates
#> [1] FALSE
#> 
#> $is_own_father_ids
#> NULL
#> 
#> $is_own_mother_ids
#> NULL
#> 
#> $duplicated_parents_ids
#> NULL
```

Great! Notice what happened here: the function was able to repair the
full duplicate, without any manual intervention. That still leaves us
with the sibling ID conflict, but that’s a more complex issue that would
require manual intervention. We’ll leave that for now.

So far we’ve only checked for violations of uniqueness. But do these
errors also affect the graph structure? Let’s find out.

### Oedipus ID

Just as Oedipus discovered his true relationship was not what records
suggested, our data can reveal its own confused parentage when an ID is
incorrectly listed as its own parent. Let’s examine this error:

Sometimes, an individual’s parents’ IDs may be incorrectly listed as
their own ID, leading to within-row duplicates. The checkIDs function
can also identify these errors:

``` r
# Create a sample dataset with within-person duplicate parent IDs

df_within <- ped2fam(potter, famID = "newFamID", personID = "personID")

df_within$momID[df_within$name == "Vernon Dursley"] <- df_within$personID[df_within$name == "Vernon Dursley"]

# Check for within-row duplicates
result <- checkIDs(df_within, repair = FALSE)
print(result)
#> $all_unique_ids
#> [1] TRUE
#> 
#> $total_non_unique_ids
#> [1] 0
#> 
#> $non_unique_ids
#> NULL
#> 
#> $total_own_father
#> [1] 0
#> 
#> $total_own_mother
#> [1] 1
#> 
#> $total_duplicated_parents
#> [1] 0
#> 
#> $total_within_row_duplicates
#> [1] 1
#> 
#> $within_row_duplicates
#> [1] TRUE
#> 
#> $is_own_father_ids
#> NULL
#> 
#> $is_own_mother_ids
#> [1] 1
#> 
#> $duplicated_parents_ids
#> NULL
```

In this example, we have created a within-row duplicate by setting the
momID of Vernon Dursley to his own ID. The `checkIDs` function correctly
identifies that this error is present.

To repair within-row duplicates, you will be able to set the repair
argument to `TRUE`, eventually. This feature is currently under
development and will be available in future versions of the package. In
the meantime, you can manually inspect and then correct these errors in
your dataset.

``` r
# Find the problematic entry

df_within[df_within$momID %in% result$is_own_mother_ids, ]
#>   personID newFamID famID           name first_name surname gen momID dadID
#> 1        1        1     1 Vernon Dursley     Vernon Dursley   1     1   102
#>   spouseID sex twinID zygosity
#> 1        3   1     NA     <NA>
```

There are several ways to correct this issue, depending on the specifics
of your dataset. In this case, you could correct the momID for Vernon
Dursley to the correct value, resolving the within-row duplicate, likely
by assuming that his sister Marjorie shares the same mother.

## Identifying and Repairing Sex Coding Issues

Another critical aspect of pedigree validation is ensuring the
consistency of sex coding. This brings us to an important distinction in
genetic studies between biological sex (genotype) and gender identity
(phenotype):

- Biological sex (genotype) refers to an individual’s chromosomal
  configuration, typically XX for female and XY for male in humans,
  though variations exist.
- Gender identity (phenotype) encompasses a broader, richer, personal,
  deeply-held sense of being male, female, a blend of both, neither, or
  another gender entirely.

The `checkSex` function focuses on biological sex coding consistency,
particularly looking for: - Mismatches between parent roles and recorded
sex - Individuals listed as both parent and child - Inconsistent sex
coding across the dataset

Let’s examine how it works:

``` r
# Validate sex coding

results <- checkSex(potter,
  code_male = 1,
  code_female = 0,
  verbose = TRUE, repair = FALSE
)
#> Standardizing column names...
#> Step 1: Checking how many sexes/genders...
#> 2 unique sex codes found: 1, 0
#> Role: dadID
#> 1  unique sex codes found:  1 
#> Modal sex code:  1 
#> All parents consistently coded.
#> Role: momID
#> 1  unique sex codes found:  0 
#> Modal sex code:  0 
#> All parents consistently coded.
#> Checks Made:
#> c(1, 0)21010numeric(0)numeric(0)numeric(0)numeric(0)
print(results)
#> $sex_unique
#> [1] 1 0
#> 
#> $sex_length
#> [1] 2
#> 
#> $all_sex_dad
#> [1] 1
#> 
#> $all_sex_mom
#> [1] 0
#> 
#> $most_frequent_sex_dad
#> [1] 1
#> 
#> $most_frequent_sex_mom
#> [1] 0
#> 
#> $ID_female_dads
#> numeric(0)
#> 
#> $ID_child_female_dads
#> numeric(0)
#> 
#> $ID_male_moms
#> numeric(0)
#> 
#> $ID_child_male_moms
#> numeric(0)
```

When inconsistencies are found, you can attempt automatic repair:

``` r
# Repair sex coding
df_fix <- checkSex(potter,
  code_male = 1,
  code_female = 0,
  verbose = TRUE, repair = TRUE
)
#> Standardizing column names...
#> Step 1: Checking how many sexes/genders...
#> 2 unique sex codes found: 1, 0
#> Role: dadID
#> 1  unique sex codes found:  1 
#> Modal sex code:  1 
#> All parents consistently coded.
#> Role: momID
#> 1  unique sex codes found:  0 
#> Modal sex code:  0 
#> All parents consistently coded.
#> Step 2: Attempting to repair sex coding...
#> Changes Made:
#> Recode sex based on most frequent sex in dads: 1. Total sex changes made:  36
print(df_fix)
#>     ID famID               name first_name  surname gen momID dadID spID sex
#> 1    1     1     Vernon Dursley     Vernon  Dursley   1   101   102    3   M
#> 2    2     1   Marjorie Dursley   Marjorie  Dursley   1   101   102   NA   F
#> 3    3     1      Petunia Evans    Petunia    Evans   1   103   104    1   F
#> 4    4     1         Lily Evans       Lily    Evans   1   103   104    5   F
#> 5    5     1       James Potter      James   Potter   1    NA    NA    4   M
#> 6    6     1     Dudley Dursley     Dudley  Dursley   2     3     1   NA   M
#> 7    7     1       Harry Potter      Harry   Potter   2     4     5    8   M
#> 8    8     1      Ginny Weasley      Ginny  Weasley   2    10     9    7   F
#> 9    9     1     Arthur Weasley     Arthur  Weasley   1    NA    NA   10   M
#> 10  10     1      Molly Prewett      Molly  Prewett   1    NA    NA    9   F
#> 11  11     1        Ron Weasley        Ron  Weasley   2    10     9   17   M
#> 12  12     1       Fred Weasley       Fred  Weasley   2    10     9   NA   M
#> 13  13     1     George Weasley     George  Weasley   2    10     9   NA   M
#> 14  14     1      Percy Weasley      Percy  Weasley   2    10     9   20   M
#> 15  15     1    Charlie Weasley    Charlie  Weasley   2    10     9   NA   M
#> 16  16     1       Bill Weasley       Bill  Weasley   2    10     9   18   M
#> 17  17     1   Hermione Granger   Hermione  Granger   2    NA    NA   11   F
#> 18  18     1     Fleur Delacour      Fleur Delacour   2   105   106   16   F
#> 19  19     1 Gabrielle Delacour  Gabrielle Delacour   2   105   106   NA   F
#> 20  20     1             Audrey     Audrey  Unknown   2    NA    NA   14   F
#> 21  21     1    James Potter II      James   Potter   3     8     7   NA   M
#> 22  22     1       Albus Potter      Albus   Potter   3     8     7   NA   M
#> 23  23     1        Lily Potter       Lily   Potter   3     8     7   NA   F
#> 24  24     1       Rose Weasley       Rose  Weasley   3    17    11   NA   F
#> 25  25     1       Hugo Weasley       Hugo  Weasley   3    17    11   NA   M
#> 26  26     1   Victoire Weasley   Victoire  Weasley   3    18    16   NA   F
#> 27  27     1  Dominique Weasley  Dominique  Weasley   3    18    16   NA   F
#> 28  28     1      Louis Weasley      Louis  Weasley   3    18    16   NA   M
#> 29  29     1      Molly Weasley      Molly  Weasley   3    20    14   NA   F
#> 30  30     1       Lucy Weasley       Lucy  Weasley   3    20    14   NA   F
#> 31 101     1     Mother Dursley     Mother  Dursley   0    NA    NA  102   F
#> 32 102     1     Father Dursley     Father  Dursley   0    NA    NA  101   M
#> 33 104     1       Father Evans     Father    Evans   0    NA    NA  103   M
#> 34 103     1       Mother Evans     Mother    Evans   0    NA    NA  104   F
#> 35 106     1    Father Delacour     Father Delacour   0    NA    NA  105   M
#> 36 105     1    Mother Delacour     Mother Delacour   0    NA    NA  106   F
#>    twinID zygosity
#> 1      NA     <NA>
#> 2      NA     <NA>
#> 3      NA     <NA>
#> 4      NA     <NA>
#> 5      NA     <NA>
#> 6      NA     <NA>
#> 7      NA     <NA>
#> 8      NA     <NA>
#> 9      NA     <NA>
#> 10     NA     <NA>
#> 11     NA     <NA>
#> 12     13       mz
#> 13     12       mz
#> 14     NA     <NA>
#> 15     NA     <NA>
#> 16     NA     <NA>
#> 17     NA     <NA>
#> 18     NA     <NA>
#> 19     NA     <NA>
#> 20     NA     <NA>
#> 21     NA     <NA>
#> 22     NA     <NA>
#> 23     NA     <NA>
#> 24     NA     <NA>
#> 25     NA     <NA>
#> 26     NA     <NA>
#> 27     NA     <NA>
#> 28     NA     <NA>
#> 29     NA     <NA>
#> 30     NA     <NA>
#> 31     NA     <NA>
#> 32     NA     <NA>
#> 33     NA     <NA>
#> 34     NA     <NA>
#> 35     NA     <NA>
#> 36     NA     <NA>
```

When the repair argument is set to `TRUE`, repair process follows
several rules: - Parents listed as mothers must be female - Parents
listed as fathers must be male - Sex codes are standardized to the
specified code_male and code_female values - If no sex code is provided,
the function will attempt to infer what male and female are coded with.
The most frequently assigned sex for mothers and fathers will be used as
the standard.

Note that automatic repairs should be carefully reviewed, as they may
not always reflect the correct biological relationships. In cases where
the sex coding is ambiguous or conflicts with known relationships,
manual inspection and domain knowledge may be required.

## Best Practices for Pedigree Validation

Through extensive work with pedigree data, we’ve learned several key
principles:

- Always inspect your data before applying automatic repairs
- Use summarizeFamilies() to get an overview of family structures
- Keep detailed records of changes made during cleaning
- Validate after each repair step
- Create backups before applying repairs
- Trust your domain knowledge - automatic repairs are helpful but not
  infallible

By following these best practices, and leveraging functions like
`checkIDs`, `checkSex`, and `recodeSex`, you can ensure the integrity of
your pedigree data, facilitating accurate analysis and research.
