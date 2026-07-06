
# Description

This update includes an extended vignette and several openmx convenience functions. We left openmx as a suggests rather than a dependency because the openmx convenience aren't essential to the core functions. This should resolves an older error in CRAN (with R 1.5.0), and instead results in r-oldrel-windows-x86_64	1.5.2	20.00	245.00	265.00	NOTE

# Test Environments

1. Local OS: Windows 11 x64 (build 26220), R 4.6.1 (2026-03-11 ucrt)
2. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/23058399384)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.

## R CMD check results


── R CMD check results ──────────────────────────────────────────────── BGmisc 1.9.0 ────
Duration: 3m 46.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package. 

 * We saw 0 new problems
 * We failed to check 0 packages
 
The development version of ggpedigree should resolve "E: 1" seen in the CRAN version. It's related to the openMx failure in older versions. I maintain both packages, so once the latest version of BGmisc is on CRAN, I will submit the updated ggpedigree version.

>pak::pkg_install("r-lib/revdepcheck")
> revdepcheck::revdep_check(num_workers = 4)
── INSTALL ────────────────────────────────────────────────────────── 2 versions ──
── CHECK ──────────────────────────────────────────────────────────── 2 packages ──
✔ discord 1.3                            ── E: 0     | W: 0     | N: 0      
✔ ggpedigree 1.1.0.3                     ── E: 1     | W: 0     | N: 0      
OK: 2                                                                               

BROKEN: 0
Total time: 6 min
