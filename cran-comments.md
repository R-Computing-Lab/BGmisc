Description
-----------------------------------------------
This is a major update to the BGmisc package, adding in several new functions related to simulating pedigrees and calculating relatedness. We've also transitioned to having Mason Garrison maintain the package.


Test environments
-----------------------------------------------
1. Local OS: Windows 10, R version 4.2.3 
2. Local OS: Windows 10, R version 4.3.1 
3. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/6240228122)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.

## R CMD check results

0 errors | 0 warnings | 0 notes



Notes
Version: 1.0
Date: 2023-09-10
Description: The BGmisc R package offers a comprehensive suite of functions tailored for extended behavior genetics analysis, including model identification, calculating relatedness, pedigree conversion, pedigree simulation, and more.
Dependencies:
Imports: Matrix, stats, kinship2, igraph
Suggests: knitr, rmarkdown, EasyMx, OpenMx, dplyr, testthat (>= 3.0.0)
Depends: R (>= 3.5.0)
