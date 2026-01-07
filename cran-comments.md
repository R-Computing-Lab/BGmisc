
# Description

This update includes minor enhancements and bug fixes related to how string ids are handled in various functions. It also now allows certain vignettes to not throw an error if openmx is not installed for older R versions.

# Test Environments

1. Local OS: Windows 11 x64 (build 26220), R 4.5.2 (2025-10-31 ucrt)
2. **GitHub Actions**:  
    - [Link](https://github.com/R-Computing-Lab/BGmisc/actions/runs/20666823859)
    - macOS (latest version) with the latest R release.
    - Windows (latest version) with the latest R release.
    - Ubuntu (latest version) with:
        - The development version of R.
        - The latest R release.

        
## R CMD check results

── R CMD check results ───────────────────────────────────────────────────────────────────────── BGmisc 1.5.2 ────
Duration: 1m 26.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

> revdepcheck::revdep_check(num_workers = 4)
── INSTALL ────────────────────────────────────────────────────────── 2 versions ──
── CHECK ──────────────────────────────────────────────────────────── 2 packages ──
✔ discord 1.2.4.1                        ── E: 0     | W: 0     | N: 0             
✔ ggpedigree 1.0.0.1                     ── E: 1     | W: 0     | N: 0             
OK: 2                                                                            

BROKEN: 0
Total time: 4 min
