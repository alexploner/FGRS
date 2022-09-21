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

#' List of diseases currently supported
fgrs_diseases <- c("SZ", "BD")
ndis <- length(fgrs_diseases)

#' Mapping of birth decades to years
breaks <- c(-Inf, seq(1930, 1990, by = 10), 2015)
ndecad <- length(breaks) - 1
fgrs_decades <- data.frame(Start = breaks[-(ndecad+1)], End = breaks[-1], BirthDecade = 1:ndecad)

#' Initialize some small constants for each disease:
#'
#' * Shrinkage factors for cohabitation effects (Table 2/Step 3 in the appendix)
#' * Shrinkage factor adjusting for number of relatives: this is directly implemented
#'   as a function with two numerical constants extracted from the SAS code, where the
#'   argument `nrel` is the weighted number of relatives for a proband (Table 2/Step 6
#'   in the appendix)
fgrs_const <- list(
  SZ = list(cohab  = list(parent_child = 0.93, sibling = 0.84),
            shrink_nrel = function(nrel) {0.00222 / (0.00222 + 0.00249062/nrel)} ),
  BD = list(cohab  = list(parent_child = 0.67, sibling = 0.77),
            shrink_nrel = function(nrel) {0.00162265 / (0.00162265 + 0.00456254/nrel)} )
)

#' Initialize containers for larger data (read from file)
fgrs_cuminc <- vector("list", ndis)
names(fgrs_cuminc) <- fgrs_diseases
fgrs_standconst <- fgrs_meanliab <- fgrs_cuminc

#' Loop over diseases: read the cumulative incidences and mean liabilities from file
for (d in fgrs_diseases) {

  ## Set up file names
  f_ci <- file.path("data-raw", d, paste0("cuminc_diag_", d, ".txt"))
  f_ml <- file.path("data-raw", d, paste0("liability_threshold_", d, ".txt"))
  f_sc <- file.path("data-raw", d, paste0("MeanSTD_", d, ".txt"))

  ## Read cumulative incidence
  fgrs_cuminc[[d]] <- read.table(f_ci, header = TRUE, sep ="\t")

  ## Read liability thresholds
  lt <- read.table(f_ml, header = TRUE, sep ="\t")
  ## Calculate the corresponding prevalences (for inspection only)
  lt$Prev <- pnorm(lt$LiabThresh, lower.tail = FALSE)

  ## Calculate mean liabilities for cases / controls based on liability thresholds
  ml <- list("0" = transform(lt, MeanLiab = truncnorm::etruncnorm(a = -Inf, b = LiabThresh)),
             "1" = transform(lt, MeanLiab = truncnorm::etruncnorm(a = LiabThresh, b = Inf))
             )
  ml <- plyr::ldply(ml, .id = "HasDiag")
  ml$HasDiag <- as.numeric( as.character( ml$HasDiag) )
  fgrs_meanliab[[d]] <- ml

  ## Read standardisation constants (mean, stddev) for scores by birthyear
  fgrs_standconst[[d]] <- read.table(f_sc, header = TRUE, sep ="\t")

}

#' Add the generated objects to the package
usethis::use_data(fgrs_diseases, fgrs_decades, fgrs_const, fgrs_cuminc,
                  fgrs_meanliab, fgrs_standconst, internal = TRUE, overwrite = TRUE)
