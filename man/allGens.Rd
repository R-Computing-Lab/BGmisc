% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateFamilySize.R
\name{allGens}
\alias{allGens}
\title{allGens
A function to calculate the number of individuals in each generation. This is a supporting function for \code{simulatePedigree}.}
\usage{
allGens(kpc, Ngen, marR)
}
\arguments{
\item{kpc}{Number of kids per couple (integer >= 2).}

\item{Ngen}{Number of generations (integer >= 1).}

\item{marR}{Mating rate (numeric value ranging from 0 to 1).}
}
\value{
Returns a vector containing the number of individuals in every generation.
}
\description{
allGens
A function to calculate the number of individuals in each generation. This is a supporting function for \code{simulatePedigree}.
}
