
# Description

This update introduces several enhancements to functionality, performance, and documentation. The package now includes faster methods for constructing adjacency matrices and comparison tests for evaluating those methods, as well as new pedigree datasets including the Royal Family and A Song of Ice and Fire. New functions have been added: com2links() for converting components to kinship links, extractWikiFamilyTree() for parsing family tree templates from wikis, and checkPedigreeNetwork() for validating pedigree network structure. The plotPedigree() function has been refined to silence unnecessary invisible list outputs. A new vignette demonstrates the use of adjacency matrix methods, and documentation has been expanded to include additional examples and clearer explanations.

## Pretest Notes

None

# Test Environments

1. Local OS: Windows 11 x64 (build 22635), R 4.4.2 (2024-10-31 ucrt)
2. Local OS: Windows 11 x64 (build 22635), R 4.4.3 (2025-02-28 ucrt)
3. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/14227584727)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.
        
## R CMD check results

==> devtools::check()

══ Documenting ═════════════════════════════════════════════════════════════════════════════════════
ℹ Updating BGmisc documentation
ℹ Loading BGmisc

══ Building ════════════════════════════════════════════════════════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
── R CMD build ─────────────────────────────────────────────────────────────────────────────────────
✔  checking for file 'E:\Dropbox\Lab\Research\Projects\2024\BGMiscJoss\BGmisc_main/DESCRIPTION' ...
─  preparing 'BGmisc': (2.6s)
✔  checking DESCRIPTION meta-information ...
─  installing the package to build vignettes
✔  creating vignettes (14.2s)
─  excluding invalid files (1.3s)
   Subdirectory 'R' contains invalid file names:
     'checkParents.X'
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
   Removed empty directory 'BGmisc/checkpoint'
─  building 'BGmisc_1.3.5.tar.gz'
   
══ Checking ════════════════════════════════════════════════════════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
• _R_CHECK_CRAN_INCOMING_                      : FALSE
• _R_CHECK_FORCE_SUGGESTS_                     : FALSE
• _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
• NOT_CRAN                                     : true
── R CMD check ─────────────────────────────────────────────────────────────────────────────────────
─  using log directory 'E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc.Rcheck'
─  using R version 4.4.3 (2025-02-28 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 13.3.0
       GNU Fortran (GCC) 13.3.0
─  running under: Windows 11 x64 (build 26120)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'BGmisc/DESCRIPTION'
─  this is package 'BGmisc' version '1.3.5'
─  package encoding: UTF-8
✔  checking package namespace information
✔  checking package dependencies (900ms)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files (1.2s)
✔  checking for hidden files and directories ...
✔  checking for portable file names
─  checking whether package 'BGmisc' can be installed ... [14s] OK (13.5s)
✔  checking installed package size ... 
✔  checking package directory
✔  checking for future file timestamps ... 
✔  checking 'build' directory
✔  checking DESCRIPTION meta-information ... 
✔  checking top-level files
✔  checking for left-over files ...
✔  checking index information ...
✔  checking package subdirectories (715ms)
✔  checking code files for non-ASCII characters ... 
✔  checking R files for syntax errors ... 
✔  checking whether the package can be loaded (707ms)
✔  checking whether the package can be loaded with stated dependencies (608ms)
✔  checking whether the package can be unloaded cleanly (709ms)
✔  checking whether the namespace can be loaded with stated dependencies (608ms)
✔  checking whether the namespace can be unloaded cleanly (709ms)
✔  checking loading without being on the library search path (646ms)
✔  checking dependencies in R code (1.7s)
✔  checking S3 generic/method consistency (710ms)
✔  checking replacement functions (608ms)
✔  checking foreign function calls (709ms)
✔  checking R code for possible problems (6.4s)
✔  checking Rd files (508ms)
✔  checking Rd metadata ... 
✔  checking Rd line widths ... 
✔  checking Rd cross-references (509ms)
✔  checking for missing documentation entries (609ms)
✔  checking for code/documentation mismatches (1.7s)
✔  checking Rd \usage sections (1s)
✔  checking Rd contents ... 
✔  checking for unstated dependencies in examples ... 
✔  checking contents of 'data' directory ...
✔  checking data for non-ASCII characters ... 
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ... 
✔  checking installed files from 'inst/doc' ... 
✔  checking files in 'vignettes' ... 
✔  checking examples (1.7s)
✔  checking for unstated dependencies in 'tests' ... 
─  checking tests ...
✔  Running 'testthat.R' (4s)
✔  checking for unstated dependencies in vignettes (369ms)
✔  checking package vignettes ... 
─  checking re-building of vignette outputs ... [15s] OK (15.5s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
── R CMD check results ─────────────────────────────────────────────────────────── BGmisc 1.3.5 ────
Duration: 1m 1.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded