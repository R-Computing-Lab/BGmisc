
# Description

This update includes several enhancements and bug fixes to the BGmisc package, improving its functionality and usability for behavior genetics analysis. The key changes include:
- **New Features**: 
  - Added `calculateCIs` function for computing confidence intervals for correlation coefficients.
  - Introduced `addPhantomParents` function to handle phantom parents more efficiently.
  - Added aliases for mitochondrial-related terms (`mtdna`, `mitochondria`).
- **Performance Improvements**:
  - Refactored `addPhantomParents` for better efficiency.
  - Optimized the `com2links`, `summarizePedigree`, and `checkIDs` functions to reduce complexity.
- **Documentation and Testing**:
  - Updated documentation to reflect new features and improvements.
  - Added comprehensive tests for the `calculateCIs` function and other new features.
  - Reorganized unit tests for better structure and clarity.


# Test Environments

1. Local OS: Windows 11 x64 (build 26120), R 4.5.0 (2025-04-11 ucrt)
2. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/15336728239)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.

        
## R CMD check results

==> devtools::check()

══ Documenting ═══════════════════════════════════════
ℹ Updating BGmisc documentation
ℹ Loading BGmisc

══ Building ══════════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
── R CMD build ───────────────────────────────────────
✔  checking for file 'E:\Dropbox\Lab\Research\Projects\2024\BGMiscJoss\BGmisc_main/DESCRIPTION' ...
─  preparing 'BGmisc': (3.8s)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (23.1s)
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
   Removed empty directory 'BGmisc/checkpoint'
─  building 'BGmisc_1.4.1.tar.gz'
   
══ Checking ══════════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
• _R_CHECK_CRAN_INCOMING_                      : FALSE
• _R_CHECK_FORCE_SUGGESTS_                     : FALSE
• _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
• NOT_CRAN                                     : true
── R CMD check ───────────────────────────────────────
─  using log directory 'E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc.Rcheck'
─  using R version 4.5.0 (2025-04-11 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 14.2.0
       GNU Fortran (GCC) 14.2.0
─  running under: Windows 11 x64 (build 26120)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'BGmisc/DESCRIPTION'
─  this is package 'BGmisc' version '1.4.1'
─  package encoding: UTF-8
✔  checking package namespace information
✔  checking package dependencies (500ms)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files (1.9s)
✔  checking for hidden files and directories ...
✔  checking for portable file names
✔  checking whether package 'BGmisc' can be installed (7.8s)
✔  checking installed package size ... 
✔  checking package directory
✔  checking for future file timestamps (707ms)
✔  checking 'build' directory ...
✔  checking DESCRIPTION meta-information (620ms)
✔  checking top-level files ... OK
✔  checking for left-over files
✔  checking index information ...
✔  checking package subdirectories (748ms)
✔  checking code files for non-ASCII characters ... 
✔  checking R files for syntax errors ... 
✔  checking whether the package can be loaded (710ms)
✔  checking whether the package can be loaded with stated dependencies (710ms)
✔  checking whether the package can be unloaded cleanly (808ms)
✔  checking whether the namespace can be loaded with stated dependencies (708ms)
✔  checking whether the namespace can be unloaded cleanly (811ms)
✔  checking loading without being on the library search path (752ms)
✔  checking dependencies in R code (1.8s)
✔  checking S3 generic/method consistency (810ms)
✔  checking replacement functions (708ms)
✔  checking foreign function calls (909ms)
─  checking R code for possible problems ... [10s] OK (10s)
✔  checking Rd files (709ms)
✔  checking Rd metadata ... 
✔  checking Rd line widths ... 
✔  checking Rd cross-references (527ms)
✔  checking for missing documentation entries (811ms)
✔  checking for code/documentation mismatches (2.3s)
✔  checking Rd \usage sections (1.3s)
✔  checking Rd contents ... 
✔  checking for unstated dependencies in examples ... 
✔  checking contents of 'data' directory ...
✔  checking data for non-ASCII characters ... 
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ... 
✔  checking installed files from 'inst/doc' ... 
✔  checking files in 'vignettes' ... 
✔  checking examples (1.8s)
✔  checking for unstated dependencies in 'tests' ... 
─  checking tests ...
✔  Running 'testthat.R' (6.8s)
✔  checking for unstated dependencies in vignettes (366ms)
✔  checking package vignettes ... 
─  checking re-building of vignette outputs ... [26s] OK (26s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory ...

── R CMD check results ───────────── BGmisc 1.4.1 ────
Duration: 1m 17.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
