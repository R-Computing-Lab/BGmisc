---
title: 'BGmisc: An R Package for Behavior Genetics Analysis'
tags:
  - R
  - pedigrees
  - behavior genetics
authors:
  - name: S. Mason Garrison
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Michael D. Hunter
    orcid: 0000-0002-3651-6709
    affiliation: 2
  - name: Xuanyu Lyu
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Jonathan D. Trattner
    orcid: 0000-0000-0000-0000
    affiliation: 3  
  - name: S. Alexandra Burt
    orcid: 0000-0000-0000-0000
    affiliation: 4
affiliations:
 - name: University, Place, USA
   index: 1
 - name: University, Place, USA
   index: 2
 - name: University, Place, USA
   index: 3
 - name: University, Place, USA
   index: 4
date: 07/21/2023
bibliography: paper.bib

---
<!--Guidance 
JOSS welcomes submissions from broadly diverse research areas. For this reason, we require that authors include in the paper some sentences that explain the software functionality and domain of use to a non-specialist reader. We also require that authors explain the research applications of the software. The paper should be between 250-1000 words. Authors submitting papers significantly longer than 1000 words may be asked to reduce the length of their paper.
Your paper should include:

A list of the authors of the software and their affiliations, using the correct format (see the example below).
A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience.
A Statement of need section that clearly illustrates the research purpose of the software and places it in the context of related work.
A list of key references, including to other software addressing related needs. Note that the references should include full names of venues, e.g., journals and conferences, not abbreviations only understood in the context of a specific discipline.
Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it.
Acknowledgment of any financial support.
-->
# Summary
<!-- example from template
The forces on stars, galaxies, and dark matter under external gravitational
fields lead to the dynamical evolution of structures in the universe. The orbits
of these bodies are therefore key to understanding the formation, history, and
future state of galaxies. The field of "galactic dynamics," which aims to model
the gravitating components of galaxies to study their structure and evolution,
is now well-established, commonly taught, and frequently used in astronomy.
Aside from toy problems and demonstrations, the majority of problems require
efficient numerical tools, many of which require the same base code (e.g., for
performing numerical orbit integration).
-->
The package is available on GitHub under the GNU General Public License at https://github.com/R-Computing-Lab/BGmisc and on the Comprehensive R Archive Network (CRAN) at https://cran.r-project.org/package=BGmisc
# Features
- Provides behavior genetic miscellaneous functions for modeling and analysis.
- Calculates the relatedness coefficient between two individuals based on their shared ancestry, using path tracing rules first described in <wright cite>.
- Infers the relatedness between two individuals based on the observed correlation between their additive genetic variance and shared environmental variance.
- Determines if a variance components model is identified and fits the estimated variance components of a model to covariance data.
- Converts a pedigree into various types of relatedness matrices, including additive genetics, mitochondrial, common nuclear, and extended environmental relatedness matrices.
- Simulates pedigrees based on parameters like the number of children per couple, the number of generations, the sex ratio of offspring, and birth rate. 

## Limitations


# Statement of need

`BGmisc` is an R package for [non-twin data, and a grab bag of behavior genetic functions]. It was designed to [do what it does]. It [describe the dependencies and compatibilities].
<!-- example from template
`Gala` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package [@astropy] (`astropy.units` and
`astropy.coordinates`). -->

`BGmisc` was designed to be used by behavior geneticists and others working with large pedigree data. It was developed as part of a <grant>, and has been used in several projects (e.g., ) and theses (cite xuanyu). Further, the technical aspects related to model identification have been described in Hunter et al. (2021). 
<!-- example from template
`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
[@gaia] by students and experts alike.

-->

# Acknowledgements
We acknowledge assistance from Carlos Santos.
The current study is supported by the National Institute on Aging (NIA), RF1-AG073189.

<!--
We acknowledge contributions from Jonathan D. Trattner, ....
-->
# References
