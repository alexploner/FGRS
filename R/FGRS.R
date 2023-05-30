#' Calculate the family genetic risk score
#'
#' Given persons of interest (probands) and genealogical and phenotypic information
#' on their relatives, calculate the family genetic risk score (FGRS) for a
#' with available population data.
#'
#' @param probands Proband information; this is a data frame of any size containing
#'                 two special columns:
#'
#'                 * ProbandID: a unique identifier column of any suitable type
#'                 * BirthYear: integer, birth year of the proband (used for
#'                              standardisation)
#'
#' @param relatives Information on relatives; this is a data frame of any size
#'                  containing these eight special columns:
#'
#'                  * ProbandID: the identifier for the proband the relative is
#'                               related to, for matching with the `probands`
#'                               data frame (i.e. should be same type); not unique.
#'                  * RelativeID: identifiers for relatives; only unique within
#'                                the same `ProbandID`-grouping (closest relationship
#'                                to proband), but not in general (may related to
#'                                multiple probands)
#'                  * Sex: integer, sex of relative (1 = male, 2 = female)
#'                  * BirthYear: numerical, birth year of the relative (used
#'                               for liability estimation)
#'                  * DiagYear: numerical, year of first diagnosis for the
#'                              phenotype of interest; missing if no diagnosis
#'                              during follow-up
#'                  * AgeEOF: numerical, age of relative at end of follow-up
#'                  * RelType: character, type of relative (currently recognized:
#'                             M = mother, F = father, Sib = sibling)
#'                  * SharedGenetics: numerical, proportion of genes shared
#'                                    (on average) with proband
#'
#' @param phenotype An object of class `FGRS_data` containing the marginal
#'                  population data required to calculate the FGRS for the
#'                  phenotype of interest; alternatively, a character expression
#'                  specifying one of the phenotypes included with the package,
#'                  see Examples below.
#'
#' @return A data frame with five columns, containing FGRS values and intermediate
#'         results from the FGRS calculations for all specified probands:
#'
#'         * ProbandID: unique identifier for probands, as specified in the column
#'                      of the same name in argument `probands`
#'         * nRelatives: number of relatives weighted by shared genetics (used in
#'                       calculating shrinkage factors)
#'         * crudeFGRS: average liability in relatives, weighted by age, shared
#'                      genetics and cohabitation status; result of Step 5 in
#'                      Kendler et al., Supp Table 2.
#'         * shrunkFGRS: crude FGRS adjusted for (weighted) number of relatives
#'                       by a shrinkage factor; result of Step 6 in Kendler et al.,
#'                       Supp Table 2.
#'         * FGRS: standardized shrunk FGRS (by birth year of proband), corresponding
#'                 to result of final Step 7 in Kendler et al., Supp Table 2. This
#'                 is the FGRS to be used for further investigations.
#'
#'         Note that the FGRS values here are *not* standardized by county of
#'         longest residence.
#'
#'         The data frame has the same number of rows and order of probands as
#'         argument `probands`, so the `FGRS`-column can be directly added to the
#'         original data frame via `cbind` if the intermediate / relative-level
#'         information is not of interest, see Examples.
#'
#'         The data frame also carries information on intermediate results of the
#'         FGRS calculations on the relative level, i.e. before averaging across
#'         relatives by proband. These intermediate results are intended for
#'         diagnosis only, and are stored as attribute `FGRS_intermediate_relatives`,
#'         which is a data frame with columns:

#'        * ProbandID: identifier for the proband the relative is related to
#'        * RelativeID: identifiers for relatives
#'        * AgeWeight: liability weight for relative, based on sex, age and
#'                     diagnostic status (phenotpye y/n) at end of follow-up;
#'                     see Kendler et al., Supp Table 2, Step 1.
#'        * CrudeLiab: estimated liability for relative, based on sex, birth year
#'                     and diagnostic status (phenotpye y/n) at end of follow-up;
#'                     see Kendler et al., Supp Table 2, Step 2.
#'        * CohabWeight: correction factor for shared environmental factors
#'                       through cohabitation; see Kendler et al., Supp Table 2,
#'                       Step 3.
#'        * LiabWeight: final liability weight for relative after combining
#'                      crude liability, age weights, cohabitation weights and
#'                      shared genetics; result of Step 4 in Kendler et al.,
#'                      Supp Table 2.
#'
#'         Formally, the return value also inherits from S3 class `FGRS_result`,
#'         but there are currently no methods defined for this class.
#'
#' @details Probands without relatives will lead to a warning and a missing value
#'          for the corresponding FGRS.
#'
#'          Relatives unrelated to any specified proband will lead to a warning
#'          and will be dropped silently from any intermediate relative-level
#'          calculations and results.
#'
#' @references Kendler, K., Ohlsson, H., Sundquist, J., & Sundquist, K. (2021).
#' Impact of comorbidity on family genetic risk profiles for psychiatric and
#' substance use disorders: A descriptive analysis. Psychological Medicine, 1-10.
#' doi:10.1017/S0033291721004268
#'
#' @seealso \code{\link[FGRS]{FGRS_data}} \code{\link[FGRS]{ex1}}
#' @import dplyr
#' @export
#' @examples
#' sz <- FGRS(ex1$probands, ex1$relatives, phenotype = "SZ")
#' sz
#'
#' # Add to original data
#' cbind(ex1$probands, sz$FGRS)
#'
#' # Relative-level intermediate resuls
#' attr(sz, "FGRS_intermediate_relatives")
FGRS <- function(probands, relatives, phenotype)
{
  ## Find the specified phenotype
  phenotype <- check_phenotype(phenotype)

  ## Use simple functions to do basic validation and simplification
  probands  <- check_probands( probands )
  relatives <- check_relatives( relatives )

  ## Check the probands and relatives against each other

  ## Drop relatives without matching probands (for good)
  rel <- subset(relatives, ProbandID %in% probands$ProbandID)
  ## Drop probands without relatives (for calculation, see return value)
  pro <- subset(probands, ProbandID %in% rel$ProbandID)

  ## Useful downstream
  rel$has_diag <- as.numeric( !is.na(rel$DiagYear))

  ## Actual calculations

  ## Step 1: age weights
  age_weights <- with(rel, phenotype$get_age_weights(byear = BirthYear, sex = Sex,
                                          age_eof = AgeEOF, has_diag = has_diag) )

  ## Step 2: mean liabilities
  mean_liab <- with(rel, phenotype$get_disease_liability(byear = BirthYear,
                                                sex = Sex, has_diag = has_diag) )

  ## Step 3: cohabitation factors
  cohab_fac <- with(rel, phenotype$get_cohab_corrfac(rel_type = RelType) )

  ## Step 4: multiply
  total_liab <- age_weights * mean_liab * cohab_fac * rel$SharedGenetics

  ## Step 5: aggregate to proband level
  crude_fgrs <- stats::aggregate(total_liab, list(rel$ProbandID), mean)[[2]]
  wgt_nrel   <- stats::aggregate(rel$SharedGenetics, list(rel$ProbandID), sum)[[2]]

  ## Step 6: calculate and applot the shrinkage factor for (weighted) family size
  shrunk_fgrs <- crude_fgrs * phenotype$get_famsize_corrfac(wgt_nrel)

  ## Step 7: standardize the fgrs by birth year of proband
  fgrs <- phenotype$standardize_fgrs(shrunk_fgrs, pro$BirthYear)

  ## Build return object: we re-match to the full size of the original proband
  ## frame (before possible exclusions)
  ndx <- match(probands$ProbandID, pro$ProbandID)
  ret <- data.frame(ProbandID  = probands$ProbandID,
                    nRelatives = wgt_nrel[ndx],
                    crudeFGRS  = crude_fgrs[ndx],
                    shrunkFGRS = shrunk_fgrs[ndx],
                    FGRS       = fgrs[ndx])

  ## Put the relative-level intermediate results into a separate data frame and
  ## add as an attribute
  attr <- data.frame(ProbandID   = rel$ProbandID,
                     RelativeID  = rel$RelativeID,
                     AgeWeight   = age_weights,
                     CrudeLiab   = total_liab,
                     CohabWeight = cohab_fac,
                     LiabWeight  = total_liab)
  attr(ret, "FGRS_intermediate_relatives") <- attr

  ## Formally, the return value has an extra S3 class
  class(ret) <- c("data.frame", "FGRS_result")

  ret
}

#' Check a data frame of probands or relatives for correctness
#'
#' Given data frames that contain information on either probands or relatives
#' as required by function `FGRS`, this function runs some minimal checks on
#' correctness and returns the subset of columns required to calculate the FGRS.
#'
#' @seealso \code{\link[FGRS]{FGRS}}
check_probands <- function(x, id = "ProbandID", byear = "BirthYear")
{
  ## Checks: data frame with unique IDs, numeric and positive birth years
  stopifnot( is.data.frame(x) )
  stopifnot( !any(duplicated(x[, id])) )
  stopifnot( is.numeric(x[, byear]) )
  stopifnot( all(x[, byear] > 0) )

  ## Extract
  x <- x[, c(id, byear)]
  colnames(x) <- c("ProbandID", "BirthYear")

  x
}

#' @rdname check_probands
check_relatives <- function(x, id = "ProbandID", id2 = "RelativeID",
                            sex = "Sex", byear = "BirthYear", dyear = "DiagYear",
                            age = "AgeEOF", reltyp = "RelType",
                            gensh = "SharedGenetics")
{
  ## Checks
  stopifnot( is.data.frame(x) )

  ## Unique relative ID per proband
  tmp <- tapply(x[, id2], x[, id], function(x) !any(duplicated(x)))
  stopifnot( all(tmp) )

  stopifnot( is.numeric(x[, byear]) )
  stopifnot( all(x[, byear] > 0) )

  stopifnot( is.numeric(x[, dyear]) )
  ndx <- !is.na(x[, dyear])
  stopifnot( all(x[ndx, dyear] > 0) )
  stopifnot( all(x[ndx, dyear] >= x[ndx, byear]) )

  stopifnot( is.numeric(x[, age]) )
  stopifnot( all(x[, age] > 0) )

  ## Missing: controlled vocabulary for RelType

  stopifnot( is.numeric(x[, gensh]) )
  stopifnot( all(x[, gensh] > 0 & x[, gensh]<=1) )

  ## Missing: compare RelType and SharedGenetics

  x <- x[ c(id, id2, sex, byear, dyear, age, reltyp, gensh)]
  colnames(x) <- c("ProbandID", "RelativeID", "Sex", "BirthYear", "DiagYear",
                   "AgeEOF", "RelType", "SharedGenetics")
  x
}

#' Check a specified phenotype
#'
#' Given either a name or an object, this function checks whether a built-in
#' data object for the name exists, or whether the object inherits from a
#' valid phenotype structure. If yes, the correct phenotype is returned,
#' otherwise an error is thrown.
#'
#' @param phenotype Either a character vector of length one (i.e. a name) or
#'                  an object inheriting from class `FGRS_data`
#'
#' @return An object of class `FGRS_data`
#'
#' @seealso \code{\link[FGRS]{FGRS}} \code{\link[FGRS]{check_probands}} \code{\link[FGRS]{FGRS_data}}
check_phenotype <- function(phenotype)
{
  if (is.character(phenotype)) {
    stopifnot( phenotype %in% names(FGRS_phenotypes) )
    phenotype <- get(paste0(phenotype, "popdata"), asNamespace("FGRS"))
  } else if (!inherits(phenotype, "FGRS_data")) {
    stop("Phenotype needs to be character code or FGRS_data object")
  }
  phenotype
}


#' Calculate partial family genetic risk scores
#'
#' Calculate variant FGRS by skipping one or several steps of the full FGRS
#' algorithm: `cumulativeGFRS` calculates partial FGRS by performing steps in
#' order, but terminating before the end; `leaveoneoutFGRS` calculates incomplete
#' FGRS by skipping each step of the algorithm in turn. Both functions return
#' a series of FGRS variants that can be used to study the properties of the
#' full FGRS, e.g. as a sensitivity analysis.
#'
#' @param probands Proband information, same as for `FGRS`
#'
#' @param relatives Information on relatives; same as for `FGRS`
#'
#' @param phenotype An object of class `FGRS_data` containing the marginal
#'                  population data required to calculate the FGRS, same as for
#'                  `FGRS`.
#'
#' @return A data frame with either eight or seven columns, containing proband
#'         IDs, weighted nubmer of relatives and six variant FGRS scores
#'
#'         * `ProbandID`: unique identifier for probands, as specified in argument
#'                       `probands`
#'         * `nRelatives`: number of relatives weighted by shared genetics
#'         * Either six or five variant FGRS scores, called either `cumFGRS0` to
#'           `cumFGRS5` or `looFGRS1` to `looFGRS5`, depending on function;
#'            see Details.
#'
#'          Probands without relatives will lead to a warning and a missing value
#'          for the corresponding FGRS. Relatives unrelated to any specified
#'          proband will lead to a warning and will be dropped.
#'
#'         This data frame does _not_ carry information on intermediate results
#'         on the relative level, as in `FGRS`
#'
#' @details For the purpose of these functions, the FGRS is calculated in six steps,
#'          numbered 0-5:
#'
#'          0. Calculation of raw liabilities based on sex and birth decade
#'             of relative
#'          1. Age correction based on sex, age and disease status at end of
#'             follow-up (in relatives)
#'          2. Correction for genetic relatedness to proband (in relatives)
#'          3. Correction for cohabitation (in relatives)
#'          4. Shrinkage based on family size (in probands)
#'          5. Standardization based on birth year (in probands)
#'
#'          Note that this is not the same order of steps as in the reference
#'          below, but makes more sense like this here.
#'
#'          Note that the algorithm includes an averaging step between Steps 3
#'          and 4 above when going from relatives to probands. This is necessary
#'          and will always be run.
#'
#'          `cumFGRS0` represents the raw liability for disease among relatives;
#'          `cumFGRS1` to `cumFGRS5` represent the output from the algorithm after
#'          running the 1st to 5th step of the algorithm - consequently,
#'          `cumFGRS5` is just the full FGRS again.
#'
#'          `looFGRS1` to `looFGRS5` represent the output from the FGRS algorithm
#'          when skipping the the 1st to 5th step.
#'
#' @references Kendler, K., Ohlsson, H., Sundquist, J., & Sundquist, K. (2021).
#' Impact of comorbidity on family genetic risk profiles for psychiatric and
#' substance use disorders: A descriptive analysis. Psychological Medicine, 1-10.
#' doi:10.1017/S0033291721004268
#'
#' @seealso \code{\link[FGRS]{FGRS}}
#' @export
#' @examples
#' sz_cum <- cumulativeFGRS(ex1$probands, ex1$relatives, phenotype = "SZ")
#' sz_cum
#'
#' sz_loo <- leaveoneoutFGRS(ex1$probands, ex1$relatives, phenotype = "SZ")
#' sz_loo
cumulativeFGRS <- function(probands, relatives, phenotype)
{
  ## Find the specified phenotype
  phenotype <- check_phenotype(phenotype)

  ## Use simple functions to do basic validation and simplification
  probands  <- check_probands( probands )
  relatives <- check_relatives( relatives )

  ## Check the probands and relatives against each other

  ## Drop relatives without matching probands (for good)
  rel <- subset(relatives, ProbandID %in% probands$ProbandID)
  ## Drop probands without relatives (for calculation, see return value)
  pro <- subset(probands, ProbandID %in% rel$ProbandID)

  ## Useful downstream
  rel$has_diag <- as.numeric( !is.na(rel$DiagYear))

  ## The FGRS container
  allFGRS <- matrix(0, nrow = nrow(pro), ncol = 6)
  colnames(allFGRS) <- paste0("cumFGRS", 0:5)

  ## Step 0: mean liabilities
  fgrs_tmp <- with(rel, phenotype$get_disease_liability(byear = BirthYear,
                                                        sex = Sex, has_diag = has_diag) )
  allFGRS[ ,1] <- stats::aggregate(fgrs_tmp, list(rel$ProbandID), mean)[[2]]

  ## Step 1: age weights
  fgrs_tmp <- fgrs_tmp * with(rel, phenotype$get_age_weights(byear = BirthYear, sex = Sex,
                                                   age_eof = AgeEOF, has_diag = has_diag) )
  allFGRS[ ,2] <- stats::aggregate(fgrs_tmp, list(rel$ProbandID), mean)[[2]]

  ## Step 2: shared genetics
  fgrs_tmp <- fgrs_tmp * rel$SharedGenetics
  allFGRS[ ,3] <- stats::aggregate(fgrs_tmp, list(rel$ProbandID), mean)[[2]]

  ## Step 3: cohabitation factors
  fgrs_tmp  <- fgrs_tmp * with(rel, phenotype$get_cohab_corrfac(rel_type = RelType) )
  allFGRS[ ,4] <- stats::aggregate(fgrs_tmp, list(rel$ProbandID), mean)[[2]]

  ## Intermediate step: we continue with the aggregated FGRS (proband level)
  fgrs_tmp <- allFGRS[ ,4]

  ## Step 4: shrink by (weighted) family size
  wgt_nrel <- stats::aggregate(rel$SharedGenetics, list(rel$ProbandID), sum)[[2]]
  fgrs_tmp <- fgrs_tmp * phenotype$get_famsize_corrfac(wgt_nrel)
  allFGRS[ ,5] <- fgrs_tmp

  ## Step 5: calculate and applot the shrinkage factor for (weighted) family size
  fgrs_tmp <- phenotype$standardize_fgrs(fgrs_tmp, pro$BirthYear)
  allFGRS[ ,6] <- fgrs_tmp

  ## Build return object: we re-match to the full size of the original proband
  ## frame (before possible exclusions)
  ndx <- match(probands$ProbandID, pro$ProbandID)
  ret <- data.frame(ProbandID  = probands$ProbandID,
                    nRelatives = wgt_nrel[ndx] )

  ret <- cbind(ret, allFGRS[ndx, ])
  ret
}

#' @rdname cumulativeFGRS
#' @export
leaveoneoutFGRS <- function(probands, relatives, phenotype)
{
  ## Find the specified phenotype
  phenotype <- check_phenotype(phenotype)

  ## Use simple functions to do basic validation and simplification
  probands  <- check_probands( probands )
  relatives <- check_relatives( relatives )

  ## Check the probands and relatives against each other

  ## Drop relatives without matching probands (for good)
  rel <- subset(relatives, ProbandID %in% probands$ProbandID)
  ## Drop probands without relatives (for calculation, see return value)
  pro <- subset(probands, ProbandID %in% rel$ProbandID)

  ## Useful downstream
  rel$has_diag <- as.numeric( !is.na(rel$DiagYear))

  ## The FGRS container
  allFGRS <- matrix(0, nrow = nrow(pro), ncol = 5)
  colnames(allFGRS) <- paste0("looFGRS", 1:5)

  ## Step 0: mean liabilities (always there)
  fgrs0 <- with(rel, phenotype$get_disease_liability(byear = BirthYear,
                                                     sex = Sex, has_diag = has_diag) )

  ## Loop and skip
  for (i in 1:5) {

    ## Initialize properly
    fgrs_tmp <- fgrs0

    ## Step 1: age weights
    if (i != 1) {
      fgrs_tmp <- fgrs_tmp * with(rel, phenotype$get_age_weights(byear = BirthYear, sex = Sex,
                                                             age_eof = AgeEOF, has_diag = has_diag) )
    }

    ## Step 2: shared genetics
    if (i != 2) {
      fgrs_tmp <- fgrs_tmp * rel$SharedGenetics
    }
    ## Step 3: cohabitation factors
    if (i != 3) {
      fgrs_tmp  <- fgrs_tmp * with(rel, phenotype$get_cohab_corrfac(rel_type = RelType) )
    }

    ## Intermediate step: we continue with the aggregated FGRS (proband level, ALWAYS)
    fgrs_tmp <- stats::aggregate(fgrs_tmp, list(rel$ProbandID), mean)[[2]]

    ## Step 4: shrink by (weighted) family size
    if (i != 4) {
      wgt_nrel <- stats::aggregate(rel$SharedGenetics, list(rel$ProbandID), sum)[[2]]
      fgrs_tmp <- fgrs_tmp * phenotype$get_famsize_corrfac(wgt_nrel)
    }

    ## Step 5: calculate and applot the shrinkage factor for (weighted) family size
    if (i != 5) {
      fgrs_tmp <- phenotype$standardize_fgrs(fgrs_tmp, pro$BirthYear)
    }

    allFGRS[, i] <- fgrs_tmp

  }

  ## Build return object: we re-match to the full size of the original proband
  ## frame (before possible exclusions)
  ndx <- match(probands$ProbandID, pro$ProbandID)
  ret <- data.frame(ProbandID  = probands$ProbandID,
                    nRelatives = wgt_nrel[ndx] )

  ret <- cbind(ret, allFGRS[ndx, ])
  ret
}



