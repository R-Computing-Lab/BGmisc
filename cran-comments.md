
# Description

This update adds functionality as well as improves documentation. The package now includes the ability to save and reload pedigree objects that are used by ped2Com, optimized generation calculation, and more tests for summarizePedigree. The documentation has been enhanced to include more examples and explanations.

## Pretest Notes

As recommended by Uwe Ligges, I've now rebuilt the package using R Under development (unstable) (2025-02-17 r87739 ucrt). This should fix 
"Author field differs from that derived from Authors@R", which is due to use of an ORCID identifier.


# Test Environments

1. Local OS: Windows 11 x64 (build 22635), R version 4.4.1 (2024-06-14 ucrt)
2. Local OS: Windows 11 x64 (build 22635), R version 4.4.3 beta (2025-02-17 r87739 ucrt)
3. Local OS: Windows 11 x64 (build 22635), R Under development (unstable) (2025-02-17 r87739 ucrt)
4. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/13376514760)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.
        
## R CMD check results

─  using log directory 'E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc.Rcheck' (697ms)
─  using R version 4.4.3 beta (2025-02-17 r87739 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 13.3.0
       GNU Fortran (GCC) 13.3.0
─  running under: Windows 11 x64 (build 26120)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran' (751ms)
✔  checking for file 'BGmisc/DESCRIPTION'
─  this is package 'BGmisc' version '1.3.3'
─  package encoding: UTF-8
✔  checking package namespace information
✔  checking package dependencies (697ms)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files (1.2s)
✔  checking for hidden files and directories ...
✔  checking for portable file names
✔  checking whether package 'BGmisc' can be installed (5.7s)
✔  checking installed package size ... 
✔  checking package directory
✔  checking for future file timestamps ... 
✔  checking 'build' directory ...
✔  checking DESCRIPTION meta-information ... 
✔  checking top-level files
✔  checking for left-over files
✔  checking index information ... 
✔  checking package subdirectories (733ms)
✔  checking code files for non-ASCII characters ... 
✔  checking R files for syntax errors ... 
✔  checking whether the package can be loaded (709ms)
✔  checking whether the package can be loaded with stated dependencies (709ms)
✔  checking whether the package can be unloaded cleanly (808ms)
✔  checking whether the namespace can be loaded with stated dependencies (709ms)
✔  checking whether the namespace can be unloaded cleanly (810ms)
✔  checking loading without being on the library search path (749ms)
✔  checking dependencies in R code (1.7s)
✔  checking S3 generic/method consistency (809ms)
✔  checking replacement functions (708ms)
✔  checking foreign function calls (911ms)
✔  checking R code for possible problems (7.1s)
✔  checking Rd files (408ms)
✔  checking Rd metadata ... 
✔  checking Rd line widths ... 
✔  checking Rd cross-references (407ms)
✔  checking for missing documentation entries (709ms)
✔  checking for code/documentation mismatches (2.1s)
✔  checking Rd \usage sections (1.1s)
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
✔  Running 'testthat.R' (4s)
✔  checking for unstated dependencies in vignettes (371ms)
✔  checking package vignettes ... 
─  checking re-building of vignette outputs ... [11s] OK (11.6s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   
── R CMD check results ────────────────────── BGmisc 1.3.3 ────
Duration: 52.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
