#' Population data required for the Kendler-Ohlsson family genetic risk score
#'
#' @description A class that captures all population data required to calculate
#' the Kendler-Ohlsson FGRS for members of the underlying population
#'
#' @references Kendler, K. S., Ohlsson, H., Sundquist, J., & Sundquist, K. (2021a).
#' Family genetic risk scores and the genetic architecture of major affective and
#' psychotic disorders in a Swedish national sample. JAMA Psychiatry, 78, 735–743.
#'
#' @references Kendler, K., Ohlsson, H., Sundquist, J., & Sundquist, K. (2021b).
#' Impact of comorbidity on family genetic risk profiles for psychiatric and
#' substance use disorders: A descriptive analysis. Psychological Medicine, 1-10.
#' doi:10.1017/S0033291721004268
#'
#' @import R6
#' @export
FGRS_data <- R6::R6Class("FGRS_data", public = list(

  ## Basic fields

  #' @field disease A character vector of length one, containing the name of the
  #'                disease for which the FGRS data has been collected
  #' @field birth_decades A data frame with three numerical columns, mapping the
  #'        codes used for birth decades to actual ranges of birth years:
  #'
  #'        * `Start`: starting year of birth decade (excluded from decade)
  #'        * `End`: end year of birth decade (included in decade)
  #'        * `BirthDecade`: numerical code for birth decade
  disease = NULL,
  birth_decades = NULL,

  #' @field age_case_cuminc Data frame with three numerical columns, containing
  #'        the cumulative incidence by age at first diagnosis for all diagnosed
  #'        cases of the disease in the underlying population, separately for
  #'        males and females; see Kendler et al. 2021(b), Supplemental Table 2,
  #'        Step 1 (note: cumulative incidences stored here are *not* adjusted
  #'        for registration effects).
  #'
  #'        * `Age`: age at diagnosis
  #'        * `PropDiag`: proportion of cases diagnosed before or at the
  #'                      corresponding age
  #'        * `Sex`: indicator for sex (1 = male, 2 = female)
  age_case_cuminc = NULL,

  #' @field bdecade_liability Data frame with six numerical columns, listing
  #'        mean libability for the disease of interest by birth decade, sex and
  #'        disease status at end of follow-up; ; see Kendler et al. 2021(b),
  #'        Supplemental Table 2, Step 2.
  #'
  #'        * `BirthDecade`: numerical code for birth decade, as defined by
  #'                         field `BirthDecade`
  #'        * `Sex`: indicator for sex (1 = male, 2 = female)
  #'        * `HasDiag`: indicator for disease status at end of follow-up (0 =
  #'                     no diagnosis, 1 = diagnosed case)
  #'        * `MeanLiab`: the estimated mean liability for disease for the
  #'                      recorded combination of borth decade, sex and disease
  #'                      status (via a truncated normal distribution)
  #'        * `LiabThresh`: the liability threshold between undiagnosed subjects
  #'                        and diagnosed subjects, based on the observed
  #'                        prevalence; uniquely defines mean liability for cases
  #'                        and controls, but only included for description.
  #'        * `Prev`: the observed prevalence of the disease for subjects with
  #'                  the recorded combination of birth decade and sex; uniquely
  #'                  defines mean liability for cases and controls, but only
  #'                  included for description.
  bdecade_liability = NULL,

  #' @field cohab_parent_child,cohab_sibling Numerical vectors of lenght one,
  #'        storing the correction factors for cohabitation effects (shared
  #'        environment) for parent-child and sibling-sibling relationships;
  #'        see Kendler et al. 2021(b), Supplemental Table 2, Step 3.
  cohab_parent_child = NULL,
  cohab_sibling = NULL,

  #' @field shrinkage_factor_A,shrinkage_factor_B Numerical vectors of length one,
  #'        holding the correction factors for the number of relatives (family
  #'        size) recorded for a proband; see Kendler et al. 2021(b),
  #'        Supplemental Table 2, Step 6.
  shrinkage_factor_A = NULL,
  shrinkage_factor_B = NULL,

  #' @field byear_standard Data frame with three numerical columns, for
  #'        standardizing the calculated FGRS values by birth year of the
  #'        proband; see Kendler et al. 2021(b), Supplemental Table 2, Step 7.
  #'        Note: this implementation does not include standardisation by county
  #'        of longest residence.
  #'
  #'        * `Byear`: birth year for the borth cohort
  #'        * `Mean`: mean un-standardized FGRS across the birth cohort
  #'        * `STD`: standard deviation of un-standardized FGRS across the birth cohort
  byear_standard = NULL,

  ## Add init-function

  #' @description Initialize an empty `FGRS_data` object
  #'
  #' @param name Character vector of length one, the name of the phenotype for
  #'             which the FGRS data was collected
  #' @param birth_decades Data frame with three numerical columns, coding for the
  #'                      birth decades used in the data: `Start`, `End`,
  #'                      `BirthDecade`. Intervals are open on the left and
  #'                      closed on the right-
  #' @param age_case_cuminc Data frame with three numerical columns, listing
  #'                        cumulative incidence of the diagnosis of interest
  #'                        among cases, by age and sex: `Age`, `PropDiag` and
  #'                        `Sex` (where 1 = male and 2 = female)
  #' @param bdecade_liability Data frame with three numerical columns, listing the liability
  #'                          threshold separating subjects with and without
  #'                          diagnosis, by birth decade and sex: `BirthDecade`
  #'                          (numerical code as for argument `birth_decades`),
  #'                          `Sex`(where 1 = male and 2 = female) and `LiabThresh`
  #'                          (the actual liability threshold)
  #' @param cohab A named numerical vector or length two, listing the correction
  #'              factors for cohabitation between parents and children
  #'              (`parent_child`) and between siblings (`siblings`)
  #' @param shrinkage A named numerical vector or length two, listing the variance
  #'                  components that define shrinkage factor accounting for the
  #'                  number of relatives recorded for a proband (names:`A` and
  #'                  `B`, as in Kendler et al. 2021b, Supp.Table 2, Step 6)
  #' @param byear_standard A data frame with three columns, listing mean and
  #'                       standard deviation of the unstandardized FGRS by
  #'                       birth year of the probands: `Byear`, `Mean`, `STD`
  #'
  #' @details This function does some hard checks for consistency which will lead
  #'          to errors if violated
  initialize = function(name, birth_decades, age_case_cuminc, bdecade_liability,
                        cohab, shrinkage, byear_standard)
  {
    ## Check birth decades
    stopifnot( all(birth_decades$Start < birth_decades$End) )

    ## Check the cumulative incidence curve
    stopifnot( all( colnames(age_case_cuminc) == c("Age", "PropDiag", "Sex")))
    stopifnot ( all( age_case_cuminc$Sex %in% 1:2) )
    stopifnot( all(age_case_cuminc$Age >= 0) )
    stopifnot( all( (age_case_cuminc$PropDiag >= 0) & (age_case_cuminc$PropDiag <= 1)) )
    ## FIXME: check for monotonously increasing cumulative incidecne for f/m

    ## Check liabilities
    stopifnot( all( colnames(bdecade_liability) == c("BirthDecade", "Sex", "LiabThresh")))
    stopifnot( all(bdecade_liability$BirthDecade %in% unique(birth_decades$BirthDecade)) )
    stopifnot ( all(bdecade_liability$Sex %in% 1:2) )

    ## Check standardizations
    stopifnot( all( colnames(byear_standard) == c("Byear", "Mean", "STD")))
    stopifnot( all(byear_standard$age > 1900) )
    stopifnot( all(byear_standard$STD > 0) )

    ## Check constants
    cohab <- cohab[c("parent_child", "siblings")]
    stopifnot( all( sapply(cohab, function(x) !is.null(x)) ) )
    stopifnot( all( sapply(cohab, function(x) x > 0) ) )
    shrinkage <- shrinkage[c("A", "B")]
    stopifnot( all( sapply(shrinkage, function(x) !is.null(x)) ) )
    stopifnot( all( sapply(shrinkage, function(x) x > 0) ) )

    ## Populate the object: just copying over
    self$disease <- name
    self$birth_decades <- bith_decades
    self$age_case_cuminc <- age_case_cuminc
    self$byear_standard  <- byear_standard
    self$cohab_parent_child <- cohab[["parent_child"]]
    self$cohab_sibling      <- cohab[["siblings"]]

    ## Expand the mean liabilities to cases and controls: mean libability
    ## above and below the threshold
    ml <- list("0" = transform(bdecade_liability, MeanLiab = truncnorm::etruncnorm(a = -Inf, b = LiabThresh)),
               "1" = transform(bdecade_liability, MeanLiab = truncnorm::etruncnorm(a = LiabThresh, b = Inf))
          )
    ml <- plyr::ldply(ml, .id = "HasDiag")
    ml$HasDiag <- as.numeric( as.character( ml$HasDiag) )
    ml <- ml[, c("BirthDecade", "Sex", "HasDiag", "MeanLiab", "LiabThresh")]
    ## Add the prevalence (for plotting etc.)
    ml$Prev <- pnorm(ml$LiabThresh, lower.tail = FALSE)
    self$Bdecade_liability <- ml

    ## ... aaaand we're done
    invisible(self)
  },

  ## Add access functions; these perform the hard work for constructing the
  ## liability weights and correction factors duirng FGRS construction

  #' @description Age weights for relatives without diagnosis
  #'
  #' Relatives that have not received a diagnosis by end of follow-up will be
  #' weighted based on the cumulative incidence of diagnosis among cases at their
  #' age at end of follow-up: in other words, a relative still un-diagnosed at
  #' older age will get more weight than one at younger age. Relatives with
  #' a diagnosis will always get full weight.
  #'
  #' @param byear Numeric vector, year of birth of relative
  #' @param sex Integer vector, indicating the sex of the relative (1 = male,
  #'            2 = female)
  #' @param age_eof Numeric vector, age of relative at end of follow-up
  #' @param has_diag Logical vector, indicating whether the relative has a
  #'                 diagnosis at end of follow-up
  #'
  #' @details Fractional ages are currently linearly interpolated, but this may
  #' change
  get_age_weights = function(byear, sex, age_eof, has_diag)
  {
    ## FIXME: these function/data definitions should be moved to initialization
    ndx_m <- self$age_case_cuminc$Sex == 1
    ## These functions interpolate linearly between specified ages in the table
    ## FIXME: this is based on the assumption that the cumulative proportions
    ## refer to diagnosis < specified age (i.e. for Age == 18, the recorded
    ## PropDiag is the cumulative incidence during [0, 18), not [0, 19)
    ## FIXME: CLARIFY THIS
    age_ci_m <- approxfun(self$age_case_cuminc$Age[ndx_m], self$age_case_cuminc$PropDiag[ndx_m], rule=1:2)
    age_ci_f <- approxfun(self$age_case_cuminc$Age[!ndx_m], self$age_case_cuminc$PropDiag[!ndx_m], rule=1:2)

    ## Specified ages are corrected for register onset
    ## FIXME: while correct (wrt to sign), simply shifting ages seems dubious
    match_age <- ifelse(byear >= 1958, age_eof, age_eof - (byear - 1958))

    ## (do age truncation here if required, see fixmes above)

    ## Interpolate the appropriate cumulative incidence
    wgt   <- rep(NA, length(sex))
    ndx_m <- sex==1
    wgt[ndx_m]  <- age_ci_m(match_age[ndx_m])
    wgt[!ndx_m] <- age_ci_f(match_age[!ndx_m])

    ## Upweight cases regardless of age
    wgt <- ifelse( has_diag, 1.0, wgt )

    wgt
  },

  #' @description Mean liabilities of relatives with and without diagnosis
  #'
  #' Relatives are assigned an underlying libabilitiy for the phenotype of
  #' interest, based on its prevalence by birth decade and sex
  #'
  #' @param byear Numeric vector, year of birth of relative
  #' @param sex Integer vector, indicating the sex of the relative (1 = male,
  #'            2 = female)
  #' @param has_diag Logical vector, indicating whether the relative has a
  #'                 diagnosis at end of follow-up

  get_disease_liability = function(byear, sex, has_diag)
  {
    ## Turn birth year into birth decade
    ## FIXME: makes this an external function
    brks    <- c(self$birth_decades$Start[1], self$birth_decades$End)
    ndx     <- findInterval(byear, brks, left.open = TRUE)
    bdecade <- self$birth_decades$BirthDecade[ndx]

    ## Create a temproray df to use merge (I know...)
    tmp  <- data.frame(Sex = sex, HasDiag = has_diag, BirthDecade = bdecade)
    tmp2 <- merge(tmp, self$birth_decades[, -(4:5)],
                  by = c("Sex", "HasDiag", "BirthDecade"),
                  sort = FALSE)

    tmp2$MeanLiab
  },

  #' @description Correction factor for cohabitation
  #'
  #' Relatives that can be assumed to generally co-habit will be down-weighted,
  #' as some of their similarity in phenotype will be due to shared environment
  #' rather than shared familial genetics. See Kendler et al. 2021b, Supp.Table 2,
  #' Step 3.
  #'
  #' @param rel_type Character vector, indicating type of relation to proband.
  #'                 Here, only values `M` (mother), `F` (father) and `SI`
  #'                 (sibling) will trigger weights different from one
  get_cohab_corrfac = function(rel_type)
  {
    ifelse( rel_type %in% c("M", "F"), self$cohab_parent_child,
              ifelse( rel_type == "Sib", self$cohab_sibling, 1)
    )
  },

  #' @description Correction factor for family size
  #'
  #' Combining libability, age weights, cohabitation correction factor and
  #' degree of relatedness and averaging them across probands for a first
  #' estimate of FGRS will be biased towards larger families. This function
  #' returns a suitable shrinkage factor.
  #'
  #' @param nrel Numeric vector containing the weighted number of relatives, where
  #'             each relative is multipled by the degree of relatedness to the
  #'             proband before summation
  get_famsize_corrfac = function(nrel)
  {
    self$shrinkage_B/(self$shrinkage_B + self$shrinkage_A/nrel)
  }

) ) ## End class definition
