
# Description

This update tweaks how one test is handled for the MKL check. ( https://www.stats.ox.ac.uk/pub/bdr/Rblas/MKL/BGmisc.out  ) We have changed the test from  expect_true(all(diag(add) == 1)) to expect_true(sum((diag(add) - 1)^2) < 1e-10). This test should work on all platforms. While we were at it, we also allowed some of the expect_equal tests to have a tolerance of 1e-10.



# Test Environments

1. Local OS: Windows 11 x64 (build 22635), R version 4.4.1 (2024-06-14 ucrt)
2. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/9555870410)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.
        - The penultimate release of R.
        
## R CMD check results

── R CMD check ────────────────────────────────────────
─  using log directory 'E:/Dropbox/Lab/Research/Projects/2024/BGMiscJoss/BGmisc.Rcheck' (719ms)
─  using R version 4.4.1 (2024-06-14 ucrt)
─  using platform: x86_64-w64-mingw32
─  R was compiled by
       gcc.exe (GCC) 13.2.0
       GNU Fortran (GCC) 13.2.0
─  running under: Windows 11 x64 (build 22635)
─  using session charset: UTF-8
─  using options '--no-manual --as-cran'
✔  checking for file 'BGmisc/DESCRIPTION'
─  this is package 'BGmisc' version '1.3.1.1'
─  package encoding: UTF-8
.... boring stuff
── R CMD check results ──────────── BGmisc 1.3.1.1 ────
Duration: 54s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
