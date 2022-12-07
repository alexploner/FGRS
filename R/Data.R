## Document data objects

#' Population data for disease phenotypes
#'
#' The FGRS package provides the background population data required for
#' calculating the FGRS for some psychiatric phenotypes in the Swedish
#' general population. `FGRS_phenotypes` is a named vector that lists the
#' names of the phenotypes currently included in the package. For each
#' phenotype, the package contains an object of class `FGRS_data` that
#' contains all necessary information; these data objects have the naming
#' convention `<code>popdata`, where the `<code>` is the 2-4 letter
#' abbreviation used as name for the phneotype in `FGRS_phenotypes`, see
#' Examples.
#'
#'
#' @references Kendler, K., Ohlsson, H., Sundquist, J., & Sundquist, K. (2021b).
#' Impact of comorbidity on family genetic risk profiles for psychiatric and
#' substance use disorders: A descriptive analysis. Psychological Medicine, 1-10.
#' doi:10.1017/S0033291721004268
#'
#' @seealso \code{\link[FGRS]{FGRS_data}}
#'
#' @examples
#' # These are the supported phenotypes
#' FGRS_phenotypes
#'
#' # These are the corresponding codes
#' names(FGRS_phenotypes)
#'
#' # These are two supported phenotypes
#' SZpopdata
#' BDpopdata
"FGRS_phenotypes"

#' @rdname FGRS_phenotypes
"SZpopdata"

#' @rdname FGRS_phenotypes
"BDpopdata"


#' Example proband and relative data
#'
#' This is a very small example of probands (for whom we want to calculate
#' an FGRS) and their relatives (whose information is used to calculate
#' the FGRS).
#'
#' @format This is a list with two entries, `probands` and `relatives`. These
#'         entries are data frames with columns as required by function `FGRS`
#'         (and documented there).
#'
#' @seealso \code{\link[FGRS]{FGRS}}
#' @examples
#' ex1
#'
#' FGRS(ex1$probands, ex1$relatives, phenotype = SZpopdata)
"ex1"
