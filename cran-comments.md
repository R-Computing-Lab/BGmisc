
# Description

This update adds functionality as well as improves documentation. The package now includes the ability to save and reload pedigree objects that are used by ped2Com, optimized generation calculation, and more tests for summarizePedigree. The documentation has been enhanced to include more examples and explanations.


# Test Environments

1. Local OS: Windows 11 x64 (build 22635), R version 4.4.1 (2024-06-14 ucrt)
2. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/13376514760)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.
        
## R CMD check results

─  using log directory 'E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc.Rcheck'
─  using R version 4.4.1 (2024-06-14 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 13.2.0
       GNU Fortran (GCC) 13.2.0
─  running under: Windows 11 x64 (build 26100)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'BGmisc/DESCRIPTION'
─  this is package 'BGmisc' version '1.3.3'
─  package encoding: UTF-8
✔  checking package namespace information
✔  checking package dependencies (3.2s)
✔  checking if this is a source package ...
✔  checking if there is a namespace
✔  checking for executable files (1s)
✔  checking for hidden files and directories ...
✔  checking for portable file names
✔  checking whether package 'BGmisc' can be installed (5.8s)
✔  checking installed package size ... 
✔  checking package directory
✔  checking for future file timestamps ... 
✔  checking 'build' directory
✔  checking DESCRIPTION meta-information ... 
✔  checking top-level files
✔  checking for left-over files ...
✔  checking index information ... 
✔  checking package subdirectories (895ms)
✔  checking code files for non-ASCII characters ... 
✔  checking R files for syntax errors ... 
✔  checking whether the package can be loaded (610ms)
✔  checking whether the package can be loaded with stated dependencies (509ms)
✔  checking whether the package can be unloaded cleanly (609ms)
✔  checking whether the namespace can be loaded with stated dependencies (508ms)
✔  checking whether the namespace can be unloaded cleanly (710ms)
✔  checking loading without being on the library search path (642ms)
✔  checking dependencies in R code (1.4s)
✔  checking S3 generic/method consistency (609ms)
✔  checking replacement functions (507ms)
✔  checking foreign function calls (607ms)
✔  checking R code for possible problems (5.2s)
✔  checking Rd files (409ms)
✔  checking Rd metadata ... 
✔  checking Rd line widths ... 
✔  checking Rd cross-references (406ms)
✔  checking for missing documentation entries (508ms)
✔  checking for code/documentation mismatches (1.6s)
✔  checking Rd \usage sections (917ms)
✔  checking Rd contents ... 
✔  checking for unstated dependencies in examples ... 
✔  checking contents of 'data' directory ...
✔  checking data for non-ASCII characters ... 
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ... 
✔  checking installed files from 'inst/doc' ... 
✔  checking files in 'vignettes' ... 
✔  checking examples (1.4s)
✔  checking for unstated dependencies in 'tests' ... 
─  checking tests ...
✔  Running 'testthat.R' (3s)
✔  checking for unstated dependencies in vignettes (352ms)
✔  checking package vignettes ... 
✔  checking re-building of vignette outputs (9.9s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   
── R CMD check results ─────────────────── BGmisc 1.3.3 ────
Duration: 47.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
