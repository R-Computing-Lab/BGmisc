
# Description

This update removes an invalid ORCID iD that was included in the package.
Thanks Kurt Hornik for pointing it out to me! I'm also using this as an excuse to push the current version of the package. It also adds a new function to import GEDCOM files, among other fun things we're presenting at BGA at the end of June.

# Test Environments

1. Local OS: Windows 11 x64 (build 22635), R version 4.4.0 (2024-04-24 ucrt)
2. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/9537687414)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.
        
## R CMD check results

── R CMD check ────────────────────────────────────────────────
─  using log directory 'E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc.Rcheck'
─  using R version 4.4.0 (2024-04-24 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 13.2.0
       GNU Fortran (GCC) 13.2.0
─  running under: Windows 11 x64 (build 22635)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'BGmisc/DESCRIPTION'
─  this is package 'BGmisc' version '1.3.1'
─  package encoding: UTF-8
.... boring stuff
── R CMD check results ────────────────────── BGmisc 1.3.1 ────
Duration: 50.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
