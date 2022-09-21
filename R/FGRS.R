#' Calculate the family genetic risk score
#'
#' Given information on persons of interest (probands) and their relatives,
#' calculate the family genetic risk score (FGRS).
#'
#' @param probands Data frame containing the proband information; see example
#'                 data set `ex1$probands` for format.
#' @param relatives Data frame containing information on relatives; see example
#'                 data set `ex1$realtives` for format.
#' @param proband_diag Diagnosis for the probands; currently, only `SZ` is valid.
#' @param relatives_diag Diagnosis for the relatives; currently, only `SZ` is valid.
#'
#' @return A list with two elements, `probands` and `relatives`, data frames
#'         containing the input data and augmented by FGRS (`probands`) and
#'         the components for calculating the FGRS (`relatives`)
#'
#' @details Still early prototype, format of data / results may change.
#' @import dplyr
#' @export
#' @examples
#' FGRS(ex1$probands, ex1$relatives)
FGRS <- function(probands, relatives, proband_diag = "SZ", relatives_diag = "SZ")
{
  ## Hard-coded data definitions FIXME: S3 class or similar?
  stopifnot( colnames(probands)  == colnames(FGRS::ex1$probands) )
  stopifnot( colnames(relatives) == colnames(FGRS::ex1$relatives) )
  stopifnot( proband_diag %in% fgrs_diseases)
  stopifnot( relatives_diag %in% fgrs_diseases)

  ## Note: weight calculations for relatives are naive, i.e. we calculate
  ## separately per row of data (ignoring the fact that relatives can appear
  ## multiple times) - probably more efficient that compressing to uniqueness,
  ## calculating and re-matching.

  probdat <- probands
  reldat  <- relatives

  ## Calculate birth decades
  brks <- c(fgrs_decades$Start[1], fgrs_decades$End)
  ndx  <- findInterval(relatives$BirthYear, brks, left.open = TRUE)
  reldat$BirthDecade <- fgrs_decades$BirthDecade[ndx]
  ## Calculate disease status
  reldat$HasDiag <- !is.na(reldat$AgeDiag)

  ## Step 1: weight based on age at end of follow-up (for relatives
  ## without diagnosis)
  ##
  ## Relatives with their own diagnosis always get weight one; relatives without
  ## diagnosis get as weight the cumulative incidence of the disease at their
  ## age at end of follow-up, with a shift in age distribution for relatives born
  ## before 1958
  match_age <- with(reldat, ifelse(BirthYear >= 1958, AgeEOF, AgeEOF - (BirthYear - 1958)))
  ## FIXME: we round the age at end of follow-up here to get integers; this is
  ## because the generic example data uses fractional ages, but it's not clear
  ## whether the original ages were rounded or truncated - probably the latter...
  ## ... does it make a difference?
  match_age <- round(match_age)
  ndx <- match(match_age, fgrs_cuminc[[relatives_diag]]$Age)
  stopifnot( !any(is.na(ndx)) )
  reldat$age_weight <- ifelse( reldat$HasDiag, 1.0, fgrs_cuminc[[relatives_diag]]$PropDiag[ndx] )

  ## Step 2: liability weight based on period effects (birth decade)
  ##
  ## FIXME: this gives negative weights - is this intended? Seem risky when
  ## multiplying this with the age weights
  reldat <- merge(reldat, fgrs_meanliab[[relatives_diag]][, -(4:5)],
                  by = c("Sex", "HasDiag", "BirthDecade"),
                  sort = FALSE)
  colnames(reldat)[ncol(reldat)] <- "period_weight"

  ## Step 3: cohabitation effects
  reldat$cohab_weight <- ifelse( reldat$RelativeType %in% c("M", "F"),
                                 fgrs_const[[relatives_diag]]$cohab$parent_child,
                                 ifelse( reldat$RelativeType == "Sib",
                                         fgrs_const[[relatives_diag]]$cohab$sibling,
                                         1 )
                                )

  ## Step 4: Calculate the pre-relative weight as product
  reldat$weight <- with(reldat, age_weight * period_weight * cohab_weight * SharedGenetics)

  ## Step 5: Aggregate over probands, link aggregates back to probands
  aggw <- group_by(reldat, ProbandID) %>% summarise(weight=mean(weight), nrel=n())
  probdat <- merge(probdat, aggw, by = "ProbandID", sort = FALSE)

  ## Step 6: adjust (shrink) based on number of relatives
  probdat$shrink_fac <- fgrs_const[[relatives_diag]]$shrink_nrel(probdat$nrel)
  probdat$weight_adj <- probdat$weight * probdat$shrink_fac

  ## Step 7: standardize by birth year
  ##
  ## This is a slight variant of the original algorithm, which standardized
  ## by both birth year and county of longest residence. However, sensitivity
  ## analyses reported in the supplement (eTable 7) show extremely high
  ## correlation with the original score when using only YoB standardisation,
  ## as well as slightly higher AUC for both SZ and BD, so after consultation
  ## with Henrik O, were are going with this simpler standardisation
  ndx <- match(probdat$BirthYear, fgrs_standconst[[proband_diag]]$Byear)
  stopifnot( !any(is.na(ndx)) )
  probdat$weight_adj_std <- with( fgrs_standconst[[proband_diag]],
                                  (probdat$weight_adj - Mean[ndx]) / STD[ndx] )

  ## Reduce, resort data frames for output
  probdat <- probdat[, c(colnames(probands), "nrel", "weight", "shrink_fac", "weight_adj", "weight_adj_std")]
  reldat  <- reldat[, c(colnames(relatives), "age_weight", "period_weight", "cohab_weight", "weight")]

  ## Return
  list(probands = probdat, relatives = reldat)
}



#' Example data for FGRS calculation
#'
#' A small synthetic data set that tracks the same diagnosis in both
#' probands and relatives
#'
#' @name ex1
#' @docType data
#' @format A list with two data frames, `probands` and `relatives`
#' @details
#'     FIXME
#'
"ex1"

