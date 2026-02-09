# Pedigree Simulator Optimization Notes

## Summary

The `simulatePedigree` function has been optimized to improve performance, particularly for large pedigrees. The optimized version is available by setting `beta = TRUE` or `beta = "optimized"`.

## Key Optimizations

### 1. Vectorized Parent Selection (Major Performance Gain)

**Location:** `buildBetweenGenerations_optimized` in R/simulatePedigree.R

**Problem:** The base version used a loop to select parent couples:
```r
for (k in seq_len(sizeGens[i - 1])) {
  if (sum(isUsedParent) / nrow_df_Ngen >= marR) break
  if (!(isUsedParent[k]) && !is.na(df_Ngen$spID[k])) {
    isUsedParent[k] <- TRUE
    # Linear search for spouse on every iteration - O(n) per iteration
    isUsedParent[df_Ngen$spID == df_Ngen$id[k]] <- TRUE
  }
}
```

This resulted in **O(n²) complexity** due to the linear spouse lookup (`df_Ngen$spID == df_Ngen$id[k]`) inside the loop.

**Solution:** Vectorized approach:
```r
# Create symmetric couple keys for all couples at once
couple_keys <- paste(
  pmin(df_Ngen$id[has_spouse], df_Ngen$spID[has_spouse]),
  pmax(df_Ngen$id[has_spouse], df_Ngen$spID[has_spouse]),
  sep = "_"
)

# Get unique couples
unique_couples <- unique(couple_keys)

# Calculate how many parent couples needed
n_parent_couples <- min(
  floor(sizeGens[i - 1] * marR / 2),
  length(unique_couples)
)

# Randomly select couples
selected_couple_keys <- sample(unique_couples, n_parent_couples)

# Mark all individuals in selected couples (vectorized)
is_parent <- has_spouse & (couple_keys %in% selected_couple_keys)
df_Ngen$ifparent[has_spouse] <- is_parent
```

This reduces complexity to **O(n)**.

**Expected Impact:** 2-10x speedup depending on pedigree size, with larger gains for bigger pedigrees.

### 2. Reduced Random Permutations

**Problem:** The base version randomly permuted the same generation data frame twice (lines 164 and 228).

**Solution:** Only permute once when needed, reducing unnecessary data frame copying.

### 3. Better Index Usage

**Problem:** Subsetting operations like `df_Fam[df_Fam$gen %in% c(i, i - 1), ]` scan the entire data frame.

**Solution:** Use pre-computed row indices: `df_Fam[c(rows_prev, rows_i), ]`

## Performance Expectations

For typical use cases:
- **Small pedigrees** (Ngen=4, kpc=3): 1.5-2x speedup
- **Medium pedigrees** (Ngen=5-6, kpc=4): 3-5x speedup
- **Large pedigrees** (Ngen=7+, kpc=5+): 5-10x speedup

The speedup is more pronounced with:
- Higher number of generations (Ngen)
- Larger generation sizes
- Higher mating rates (marR)

## Usage

```r
# Use optimized version
set.seed(42)
ped <- simulatePedigree(
  kpc = 4,
  Ngen = 6,
  sexR = 0.5,
  marR = 0.7,
  beta = TRUE  # or beta = "optimized"
)

# Use base version (for comparison or debugging)
ped_base <- simulatePedigree(
  kpc = 4,
  Ngen = 6,
  sexR = 0.5,
  marR = 0.7,
  beta = FALSE  # or beta = "base" or beta = "original"
)
```

## Testing

Run the benchmark script to compare performance:
```r
source("benchmark_simulator.R")
```

Run the test suite to verify correctness:
```r
devtools::test(filter = "simulatePedigree")
```

## Future Optimization Opportunities

1. **Within-generation coupling**: The loop in `buildWithinGenerations_base` (lines 178-211) could potentially be vectorized
2. **Pre-allocation**: Some vectors could be pre-allocated with known sizes
3. **Memory efficiency**: Consider using matrices instead of data frames for intermediate calculations
4. **Parallel generation building**: Generations could potentially be built in parallel for very large pedigrees

## Backward Compatibility

- The optimized version produces equivalent results to the base version (same statistical properties)
- Due to random sampling differences in the implementation, the exact individuals selected may differ even with the same seed, but the statistical properties (sex ratios, mating rates, family structure) remain identical
- All function signatures and parameters remain unchanged
- Default behavior (`beta = FALSE`) remains the same for backward compatibility
