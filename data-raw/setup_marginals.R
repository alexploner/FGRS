#' setup_marginals.R
#'
#' Read all required marginal parameters (age distribution, disease prevalences etc.)
#' from file and store them as an internal object that is available for caclulation
#'
#' Files for different diseases are read from different sub-directories, making
#' this fairly striaghtfoward to generalize.
#'
#' Marginal distribution data has been extracted from SAS code graciously provided by
#' Henrik Ohlsson (henrik.ohlsson@med.lu.se)
#'
#' Code assumes that the the working directory is the package root directory
#'
#' Alexander.Ploner@ki.se 2022-08-05


#' Packages
library(truncnorm)  ## Required for mean from truncated normals
library(plyr)

#' Read class definitions from main R line
source("R/FGRS_classes.R")

#' List of diseases currently supported
FGRS_phenotypes <- c(SZ = "Schizophrenia", BD = "Bipolar Disorder")
npheno <- length(FGRS_phenotypes)

#' Local helper function: read a table of small constants, set up for easy
#' extraction
get_constants <- function(fn)
{
  fn  <- file.path("data-raw/const", fn)
  ret <- read.table(fn, header = TRUE, sep = "\t")
  rownames(ret) <- ret$phenotype
  ret[names(FGRS_phenotypes), ]
}

#' Local helper function: read all files related to a phenotype
get_pheno_data <- function(code)
{
  ## Set up file names
  f_ci <- file.path("data-raw", code, paste0("cuminc_diag_", code, ".txt"))
  f_ml <- file.path("data-raw", code, paste0("liability_threshold_", code, ".txt"))
  f_sc <- file.path("data-raw", code, paste0("MeanSTD_", code, ".txt"))

  ## Read cumulative incidence
  ret <- list()
  ret$cuminc     <- read.table(f_ci, header = TRUE, sep ="\t")
  ret$meanliab   <- read.table(f_ml, header = TRUE, sep ="\t")
  ret$standconst <- read.table(f_sc, header = TRUE, sep ="\t")

  ret
}

#' Mapping of birth decades to years
breaks <- c(-Inf, seq(1930, 1990, by = 10), 2015)
ndecad <- length(breaks) - 1
FGRS_decades <- data.frame(Start = breaks[-(ndecad+1)], End = breaks[-1], BirthDecade = 1:ndecad)

#' Read in small constants for cohabitation, family size
#'
#' * Correction factors for cohabitation effects, directly taken from
#'   Table 2/Step 3 in the appendix of Kendler et al. 2021
#'
#' * Shrinkage factor adjusting for number of relatives: from the SAS code,
#'   definition as described in Table 2/Step 6
cohab  <- get_constants("Cohabitation.txt")
shrink <- get_constants("Shrinkage.txt")

#' Read the marginal distribution data. This is currently done manually instead
#' of a loop, as there are only few phenotypes, some of which require
#' manual adjustment of the data

#' Schizophrenia
tmp <- get_pheno_data("SZ")
#' Non-monotonous cumulative incidence, see
#' library(ggplot)
#' ggplot(tmp$cuminc, aes(x=Age, y = PropDiag, linetype = factor(Sex), group = Sex)) + geom_line()
#'
#' Fix this: simple cumulative max
tmp$cuminc$PropDiag <- with(tmp$cuminc, unlist(tapply(PropDiag, Sex, cummax)))
#' Do the object
SZpopdata <- FGRS_data$new(name              = "SZ",
                           birth_decades     = FGRS_decades,
                           age_case_cuminc   = tmp$cuminc,
                           bdecade_liability = tmp$meanliab,
                           byear_standard    = tmp$standconst,
                           cohab             = unlist(cohab["SZ", 2:3]),
                           shrinkage         = unlist(shrink["SZ", 2:3]))

#' Bipolar disorder
tmp <- get_pheno_data("BD")
BDpopdata <- FGRS_data$new(name              = "BD",
                           birth_decades     = FGRS_decades,
                           age_case_cuminc   = tmp$cuminc,
                           bdecade_liability = tmp$meanliab,
                           byear_standard    = tmp$standconst,
                           cohab             = unlist(cohab["BD", 2:3]),
                           shrinkage         = unlist(shrink["BD", 2:3]))

#' Add the generated objects to the package
usethis::use_data(FGRS_phenotypes, SZpopdata, BDpopdata, overwrite = TRUE)
