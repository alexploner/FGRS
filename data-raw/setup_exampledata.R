#' setup_exampledata.R
#'
#' Adds some small artificial data sets with individual level data, for testing and
#' demo purposes. The format builds and expands on what is described in the appendix
#' to the original publication (Columns 1-7)
#'
#' Alexander.Ploner@ki.se 2022-08-05


#' Template: proband data - not described explicitly in appendix
probands_template <- data.frame(
  ProbandID  = numeric(0),
  BirthYear = numeric(0)
)
relatives_template <- data.frame(
  ProbandID  = numeric(0),
  RelativeID = numeric(0),
  Sex = numeric(0),
  BirthYear = numeric(0),
  DiagYear = numeric(0),
  AgeEOF = numeric(0),
  RelType = character(0),
  SharedGenetics = numeric(0)

)


#' Example 1:
#' ==========

demo_probands  <- probands_template
demo_relatives <- relatives_template

#' Proband 1
demo_probands[1, ]  <- list(1, 1975)
#' Has father, mother, sibling, all without diagnosis
demo_relatives[1, ] <- list(1, 11, 1, 1950, NA, 63.20, "F", 0.5)
demo_relatives[2, ] <- list(1, 12, 2, 1953, NA, 60.30, "M", 0.5)
demo_relatives[3, ] <- list(1, 13, 2, 1977, NA, 36.15, "Sib", 0.5)

#' Proband 2
demo_probands[2, ]  <- list(2, 1968)
#' Has father, mother, cousin first degree, cousin 2nd degree without diagnosis
#' Has sister with diagnosis
demo_relatives[4, ] <- list(2, 21, 1, 1938,   NA, 75.12, "F", 0.5)
demo_relatives[5, ] <- list(2, 22, 2, 1944,   NA, 69.55, "M", 0.5)
demo_relatives[6, ] <- list(2, 23, 2, 1970,   NA, 43.00, "Cousin", 0.25)
demo_relatives[7, ] <- list(2, 24, 1, 1980,   NA, 33.32, "Cousin2", 0.125)
demo_relatives[8, ] <- list(2,  3, 2, 1971, 1989, 42.72, "Sib", 0.5)

#' Proband 3 is sister of Proband 2: same relatives, except with brother substituted
demo_probands[3, ]  <- list(3, 1971)
demo_relatives[ 9, ] <- list(3, 31, 1, 1938,   NA, 75.12, "F", 0.5)
demo_relatives[10, ] <- list(3, 32, 2, 1944,   NA, 69.55, "M", 0.5)
demo_relatives[11, ] <- list(3, 33, 2, 1970,   NA, 43.00, "Cousin", 0.25)
demo_relatives[12, ] <- list(3, 34, 1, 1980,   NA, 33.32, "Cousin2", 0.125)
demo_relatives[13, ] <- list(3,  2, 2, 1968, 1988, 45.88, "Sib", 0.5)

#' Proband 4: f, has sib, cousin, cousin2, cousin3, all without diagnosis,
#' plus one son with a diagnosis
demo_probands[4, ]  <- list(4, 1955)
demo_relatives[14, ] <- list(3, 41, 1, 1957,   NA, 56.22, "Sib", 0.5)
demo_relatives[15, ] <- list(3, 42, 2, 1960,   NA, 53.00, "Cousin", 0.25)
demo_relatives[16, ] <- list(3, 43, 1, 1954,   NA, 59.56, "Cousin2", 0.125)
demo_relatives[17, ] <- list(3, 44, 2, 1966,   NA, 47.15, "Cousin3", 0.0625)
demo_relatives[18, ] <- list(3, 45, 1, 1979, 1999, 34.82, "Child", 0.5)

#' Combine
ex1 <- list(probands = demo_probands, relatives = demo_relatives)


#' Store example data
#' ==================

usethis::use_data(ex1, internal = FALSE, overwrite = TRUE)








