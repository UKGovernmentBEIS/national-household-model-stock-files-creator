#' ---
#' title: "Scotland survey stock test results"
#' author: "Centre for Sustainable Energy"
#' output: pdf
#' toc: true
#' ---

#+ child=c('scottishsurveyprocessing.R','test-results.R')

#' #Stock comparison
#+ eval=TRUE, echo=FALSE,warning = FALSE, message = FALSE, comment = NA
scotland.outputs <- file.path(dirname(getwd()), "outputs","scotland")
scotland.survey <- file.path(dirname(getwd()), "data","SHCS_11-13","external_cse_data.sav")
shcs <- read.spss(scotland.survey, to.data.frame = TRUE, reencode='utf-8')
shcs <- clean.survey(shcs)
cases <- read.csv(file.path(scotland.outputs,"cases.csv"), header = TRUE)

#+ eval=TRUE
print("Compare shcs and nhm built form data")
knitr::kable(test.builtform(shcs, cases))

cases <- read.csv(file.path(scotland.outputs,"cases.csv"), header = TRUE)
print("Compare shcs and nhm built form data")
knitr::kable(test.builtform(shcs, cases))
print("Compare shcs and nhm build year data (values banded in ten year intervals)")
knitr::kable(test.builtyear(shcs, cases))
print("Compare shcs and nhm region data")
knitr::kable(test.region(shcs, cases))
print("Compare shcs and nhm rurality data")
knitr::kable(test.rurality(shcs, cases))
print("Compare shcs and nhm number of bedrooms data")
print(test.numberbedrooms(shcs, cases))
print("Compare shcs and nhm number of habitable rooms data")
print(test.numberhabitablerooms(shcs, cases))
print("Compare shcs and nhm tenure data")
print(test.tenure(shcs, cases))
print("Compare shcs and nhm ground floor type data")
print(test.groundfloortype(shcs,cases))