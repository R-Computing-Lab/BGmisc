---
title: 'BGmisc: An R Package for Extended Behavior Genetics Analysis'
output:
  rmarkdown::html_vignette:
    keep_md: TRUE
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
    affiliation: 1  
  - name: S. Alexandra Burt
    orcid: 0000-0000-0000-0000
    affiliation: 3
affiliations:
 - name: Wake Forest University, North Carolina, USA
   index: 1
 - name: Pennsylvania State University, Pennsylvania, USA
   index: 2
 - name: Michigan State University, Michigan, USA
   index: 3
date: "30 August, 2023"
bibliography: paper.bib
vignette: >
  %\VignetteEncoding{UTF-8}
  %\VignetteIndexEntry{modelingandrelatedness}
  %\VignetteEngine{knitr::rmarkdown}
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
<!-- > A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience. -->

Behavior genetics focuses on understanding genetic and environmental influences on individual differences, traditionally through twin studies. With the expansion of research to more complex data structures like extended family data, there arises a need for specialized software tools. The `BGmisc` package fills this gap by offering a suite of functions specifically tailored for modeling such data.


# Statement of need
<!-- A Statement of need section that clearly illustrates the research purpose of the software and places it in the context of related work. -->
As behavior genetics delves into more complex data structures like pedigrees, the limitations of current tools become evident. 
The `BGmisc` R package is designed specifically to address these challenges, going beyond what is available in tools like `EasyMx` and `OpenMx` that mainly focus on classical twin models.


Two widely used R packages in behavior genetics modeling are `EasyMx` [@easy] and `OpenMx` [@Neale2016] . The `OpenMx` [@Neale2016] package is a workhorse in behavior genetic research. Not only is it  a general purpose software for structural equation modeling that is popular among behavior geneticists [@Garrison2018], but also for its unique features -- the `mxCheckIdentification()` function. This function checks whether a model is identified^[Sentence on what is model identification]. Conversely, `EasyMx` is a more user-friendly package that streamlines the process of building and estimating structural equation models.  It seamlessly integrates with `OpenMx`'s infastructure. Its functionalities range from foundational matrix builders like `emxCholeskyVariance` and `emxGeneticFactorVariance` to more specialized functions like `emxTwinModel` designed for  classical twin models. 

Despite their strengths, both `EasyMx` and `OpenMx` have limitations when it comes to handling extended family data. Notably, they lack functions for handling modern molecular designs [@kirkpatrick_combining_2021], modeling genetic complex relationships, inferring relatedness, or simulating pedigrees. Additionally, they can be computationally inefficient when dealing with large pedigrees.


## Features

The `BGmisc` package offers an array of features tailored for in-depth behavior genetics analysis, organized into distinct categories for clarity:

### Pedigree Analysis and Simulation:

- Relatedness Coefficient Calculation: Using path tracing rules first described in [@Wright1922], `BGmisc` calculates the relatedness coefficient between all pairs of individuals based on mother and father identifiers.

- Pedigree Conversion: `BGmisc` converts pedigrees into various relatedness matrices, including additive genetics, mitochondrial, common nuclear, and extended environmental relatedness matrices.

- Pedigree Simulation: `BGmisc` simulates pedigrees based on parameters like the number of children per couple, generations, sex ratio, and birth rate. 


### Modeling and Relatedness:

- Relatedness Inference: `BGmisc` infers the relatedness between two groups based on their observed total correlation, given  additive genetic  and shared environmental parameters.

- Model Identification: `BGmisc` evaluates whether a variance components model is identified and fits the model's estimated variance components to observed covariance data. The technical aspects related to model identification have been described in @hunter_analytic_2021.

<!-- Mention (if applicable) a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it.-->
These tools collectively provide a valuable resource for behavior geneticists and others working with extended family data. Developed as part of a grant, it has been used in several ongoing projects [@lyu_statistical_power_2023; @hunter_modeling_2023; @garrison_analyzing_2023; @burt_mom_genes_2023] and theses [@lyu_masters_thesis_2023]. 


# Availability

The `BGmisc` package is open-source and available on both GitHub at https://github.com/R-Computing-Lab/BGmisc and the Comprehensive R Archive Network (CRAN)  at https://cran.r-project.org/package=BGmisc. It is licensed under the GNU General Public License


# Acknowledgements

The current research is supported by the National Institute on Aging (NIA), RF1-AG073189. We would like to acknowledge assistance from Carlos Santos.


# References
