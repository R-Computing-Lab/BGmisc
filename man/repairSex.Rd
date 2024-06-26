% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkSex.R
\name{repairSex}
\alias{repairSex}
\title{Repairs Sex Coding in a Pedigree Dataframe}
\usage{
repairSex(ped, verbose = FALSE, code_male = NULL)
}
\arguments{
\item{ped}{A dataframe representing the pedigree data with a 'sex' column.}

\item{verbose}{A logical flag indicating whether to print progress and validation messages to the console.}

\item{code_male}{The current code used to represent males in the 'sex' column.}
}
\value{
A modified version of the input data.frame \code{ped}, containing an additional or modified 'sex_recode' column where the 'sex' values are recoded according to \code{code_male}. NA values in the 'sex' column are preserved.
}
\description{
This function serves as a wrapper around `checkSex` to specifically handle
the repair of the sex coding in a pedigree dataframe.
}
\details{
This function uses the terms 'male' and 'female' in a biological context, based on chromosomes and other biologically-based characteristics relevant to genetic studies.
This usage is not intended to negate the personal gender identity of any individual.

We recognize the importance of using language and methodologies that affirm and respect all gender identities.  While this function focuses on chromosomal information necessary for constructing genetic pedigrees, we affirm that gender is a spectrum, encompassing a wide range of identities beyond the binary.
The developers of this package express unequivocal support for folx in the transgender and LGBTQ+ communities. We respect the complexity of gender identity and acknowledge the distinction between the biological aspect of sex used for genetic analysis (genotype) and the broader, richer concept of gender identity (phenotype).
}
\examples{
\dontrun{
ped <- data.frame(ID = c(1, 2, 3), sex = c("M", "F", "M"))
repairSex(ped, code_male = "M", verbose = TRUE)
}
}
\seealso{
\code{\link{checkSex}}
}
