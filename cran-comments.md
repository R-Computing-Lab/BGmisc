
# Description

This is a hotfix for the BGmisc package, as we discovered that the plotPedigree wrapper function broke for pedigrees that contained multiple families.

# Test Environments

1. Local OS: Windows 10, R version 4.2.3
2. Local OS: Windows 10, R version 4.3.1
3. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/6317831880)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.

## R CMD check results

──────────────────────── BGmisc 1.0.1 ────
Duration: 1m 1.4s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
