#' ---
#' title: "Scotland survey stock test functions"
#' author: "Centre for Sustainable Energy"
#' output: pdf
#' toc: true
#' ---

#' Creates a generic crosstab function to use in the majority of tests
#' 
#' TODO: ADD WEIGHTS TO THE REPORTING TABLES
#' 
test.xtab <- function(shcs, nhm.data, shcs.value, dto.value) {
  shcs <- data.frame(aacode = shcs$uprn_new, weight = shcs$laWghtP, shcs.value = shcs.value)
  nhm.data <- data.frame(aacode = nhm.data$aacode, dto.value = dto.value)
  merged <- merge(shcs, nhm.data)
  crosstab <- ddply(merged, .(shcs.value, dto.value), summarize, unweighted.cases.count = length(aacode), weighted.count = round(sum(weight),0))
  totals <- data.frame(shcs.value = "ALL CASES",
                       dto.value = "TOTAL",
                       unweighted.cases.count = sum(crosstab$unweighted.cases.count),
                       weighted.count = sum(crosstab$weighted.count))
  rbind(crosstab, totals)
}

# TEMPLATE TO USE IN THE TEST FOR GENERAL COMPARISONS OF NHM AND SHCS VARIABLES

#' Produces a cross-tab comparing the SHCSVARIABLE in the SHCS with the NHMVARIABLE 
#' produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and XXX from the SHCS
#' @param cases A dataframe containing the CSVNAME file produced from the SHCS
#' @return A dataframe containing a crosstab of input XXX against output VARIABLE
#test.NAME <- function(shcs, CSV) {
#  test.xtab(
#    shcs,
#    CSV,
#    shcs$,
#    CSV$)
#}

# dwelling characteristics

#' Produces a cross-tab comparing the builtform in the SHCS with the built form 
#' produced in an NHM table
#' @param shcs A dataframe containing at least uprn_new, C1 and C2 from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input C1+C2 against output builtform
test.builtform <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    paste(shcs$C1, shcs$C2, sep = " - "),
    cases$builtform)
}

#' Produces a cross-tab comparing the buildyear in the SHCS with the builtyear 
#' produced in an NHM table
#' @param shcs A dataframe containing at least uprn_new and C4 from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input C4 against output builtyear
test.builtyear <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    gsub("\\(([0-9]*)\\,([0-9]*)\\]"
         ,cut(shcs$C4, seq(1620,2020,by = 10), dig.lab = 4)
         ,replacement="\\1 - \\2"),
    gsub("\\(([0-9]*)\\,([0-9]*)\\]"
         ,cut(cases$buildyear, seq(1620,2020,by = 10), dig.lab = 4)
         ,replacement="\\1 - \\2"))
}

#' Produces a cross-tab comparing the rururb in the SHCS with the NHMVARIABLE produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and rururb from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input rururb against output morphology
test.rurality <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$rururb,
    cases$morphologytype)
}

#TESTS TO WRITE:
#numofhabitalrooms - J1
#' Produces a cross-tab comparing the numofhabitalrooms in the SHCS with the number of habitable rooms produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and J1 from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input J1 against output numofhabitalrooms
test.numberhabitablerooms <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$J1,
    cases$numofhabitalrooms)
}

#numofbedrooms - bedrooms
#' Produces a cross-tab comparing the numofbedrooms in the SHCS with the number of habitable rooms produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and bedrooms from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input bedrooms against output numofbedrooms
test.numberbedrooms <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$bedrooms,
    cases$numofbedrooms)
}

#regiontype - la
#' Produces a cross-tab comparing the local authority in the SHCS with the corresponding region produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and la from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input la against output regiontype
test.region <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$la,
    cases$regiontype)
}

# dwellings fabric information 

#wallconstruction type
# NOTE: THIS TEST IS STILL UNDER CONSTRUCTION
#' Produces a cross-tab comparing the loft insulation thickness variables in the SHCS with that produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new, loftins and M26 from the SHCS
#' @param roofs A dataframe containing the roofs file produced from the SHCS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.externalwallconstructiontype <- function(shcs, elevations) {
  test.xtab(
    shcs,
    elevations,
    paste(shcs$Q2, shcs$Q3, sep = " - "),
    elevations$externalwallconstructiontype)
}

#wall insulation details
test.externalwalltypeandinsulation <- function(shcs, wallinsulation) {
  test.xtab(
    shcs,
    wallinsulation,
    paste(shcs$Q2, shcs$Q3, shcs$Q8, sep = " - "),
    wallinsulation$wallandinsulation)
}

#loft insulation levels
#' Produces a cross-tab comparing the loft insulation thickness variables in the SHCS with that produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new, loftins and M26 from the SHCS
#' @param roofs A dataframe containing the roofs file produced from the SHCS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.loftinsulationthickness <- function(shcs, roofs) {
  test.xtab(
    shcs,
    roofs,
    paste(shcs$loftins, shcs$M26, sep = " - "),
    roofs$insulationthickness)
}

#loft insulation levels
#' Produces a cross-tab comparing the loft insulation thickness variables in the SHCS with that produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new, loftins and M26 from the SHCS
#' @param roofs A dataframe containing the roofs file produced from the SHCS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.rooftype <- function(shcs, roofs) {
  test.xtab(
    shcs,
    roofs,
    shcs$Q19,
    roofs$structuretype)
}

#heating fuel type
#' Produces a cross-tab comparing the main space heating fuel in the SHCS with that produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and M5 from the SHCS
#' @param spaceheating A dataframe containing the space-heating file produced from the SHCS
#' @return A dataframe containing a crosstab of input M5 against output mainheatingfuel
test.mainheatingfuel <- function(shcs, space_heating) {
  test.xtab(
    shcs,
    space_heating,
    shcs$M5,
    space_heating$mainheatingfuel)
}

#heating system
#' Produces a cross-tab comparing the main space heating system type in the SHCS with that produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and M2 from the SHCS
#' @param spaceheatingsystem A dataframe containing the space-heating file produced from the SHCS
#' @return A dataframe containing a crosstab of input M2 against output spaceheatingsystemtype
test.spaceheatingsystemtype <- function(shcs, space_heating) {
  test.xtab(
    shcs,
    space_heating,
    shcs$M2,
    space_heating$spaceheatingsystemtype)
}

#lightingfraction (fraction - )
#' Produces a cross-tab comparing the low energy lighting fraction in the SHCS with that produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and L18 from the SHCS
#' @param lighting A dataframe containing the lighting file produced from the SHCS
#' @return A dataframe containing a crosstab of input N1_E against output groundfloortype
test.lighting <- function(shcs, lighting) {
  test.xtab(
    shcs,
    lighting,
    shcs$L18,
    lighting$fraction)
}

#groundfloor construction type
#' Produces a cross-tab comparing the ground floor construction type in the SHCS with the grndfloortype produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and N1_E from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input N1_E against output groundfloortype
test.groundfloortype <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$N1_E,
    cases$grndfloortype)
}

# socio-dems characteristics

#tenure
#' Produces a cross-tab comparing the tenure in the SHCS with the tenure produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and tenure from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input tenure against output tenure
test.tenure <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$tenure,
    cases$tenure)
}

#adults
#' Produces a cross-tab comparing the number of adults in the SHCS with the built form produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and numadults from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input numadults against output adults
test.adults <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$numadult,
    cases$adults)
}

# children
#' Produces a cross-tab comparing the number of children in the SHCS with the number of children produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and numchild from the SHCS
#' @param cases A dataframe containing the cases file produced from the SHCS
#' @return A dataframe containing a crosstab of input numchild against output children
test.children <- function(shcs, cases) {
  test.xtab(
    shcs,
    cases,
    shcs$numchild,
    cases$children)
}

#income
#' Produces a cross-tab comparing the annual net income in the SHCS with the household income variable produced
#' in an NHM table
#' @param shcs A dataframe containing at least uprn_new and annhhinc from the SHCS
#' @param cases A dataframe containing the occupants file produced from the SHCS
#' @return A dataframe containing a crosstab of input annhhinc against output householdincomebeforetax
test.income <- function(shcs, occupants) {
  test.xtab(
    shcs,
    occupants,
    cut(shcs$annhhinc, seq(from = -10000, to = 1000000, by = 5000), dig.lab = 6),
    cut(occupants$householdincomebeforetax, seq(from = -10000, to = 1000000, by = 5000), dig.lab = 6))
}
