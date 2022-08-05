#' setup_exampledata.R
#'
#' Adds a small artificial data set with individual level data, for testing and
#' demo purposes. The format builds and expands on what is described in the appendix
#' to the original publication (Columns 1-7)
#'
#' Alexander.Ploner@ki.se 2022-08-05

#' Template: proband data - not described explicitly in appendix
probands_template <- data.frame(
  ProbandID  = numeric(0),
  BirthYear = numeric(0),
  Sex = numeric(0),
  AgeDiag = numeric(0),
  AgeEOF = numeric(0),
  LongestCounty = numeric(0)
)
relatives_template <- data.frame(
  ProbandID  = numeric(0),
  RelativeID = numeric(0),
  SharedGenetics = numeric(0),
  BirthYear = numeric(0),
  Sex = numeric(0),
  AgeDiag = numeric(0),
  AgeEOF = numeric(0),
  RelativeType = character(0)
)

demo_probands  <- probands_template
demo_relatives <- relatives_template

#' Proband 1
demo_probands[1, ]  <- list(1, 1975, 1, 1995, 38.25, NA)
#' Has father, mother, sibling, all without diagnosis
demo_relatives[1, ] <- list(1, 11, 0.5, 1950, 1, NA, 63.20, "F")
demo_relatives[2, ] <- list(1, 12, 0.5, 1953, 2, NA, 60.30, "M")
demo_relatives[3, ] <- list(1, 13, 0.5, 1977, 2, NA, 36.15, "Sib")

#' Proband 2
demo_probands[2, ]  <- list(2, 1968, 1, 1988, 45.88, NA)
#' Has father, mother, cousin first degree, cousin 2nd degree without diagnosis
#' Has sister with diagnosis
demo_relatives[4, ] <- list(2, 21, 0.5,   1938, 1,   NA, 75.12, "F")
demo_relatives[5, ] <- list(2, 22, 0.5,   1944, 2,   NA, 69.55, "M")
demo_relatives[6, ] <- list(2, 23, 0.25,  1970, 2,   NA, 43.00, "Cousin")
demo_relatives[7, ] <- list(2, 24, 0.125, 1980, 1,   NA, 33.32, "Cousin2")
demo_relatives[8, ] <- list(2,  3, 0.5,   1971, 2, 1989, 42.72, "Sib")

#' Proband 3 is sister of Proband 2: same relatives, except with brother substituted
demo_probands[3, ]  <- list(3, 1971, 2, 1989, 42.72, NA)
demo_relatives[ 9, ] <- list(3, 21, 0.5,   1938, 1,   NA, 75.12, "F")
demo_relatives[10, ] <- list(3, 22, 0.5,   1944, 2,   NA, 69.55, "M")
demo_relatives[11, ] <- list(3, 23, 0.25,  1970, 2,   NA, 43.00, "Cousin")
demo_relatives[12, ] <- list(3, 24, 0.125, 1980, 1,   NA, 33.32, "Cousin2")
demo_relatives[13, ] <- list(3,  2, 0.5,   1968, 2, 1988, 45.88, "Sib")



4: f,m, sib with diag, cousin, cousin2, cousin3
5: is sib of 4, has same relatives f, m, sib with diag (=4), cousin, cousin2, cousin3






