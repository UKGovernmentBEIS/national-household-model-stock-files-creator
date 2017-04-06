#' ---
#' title: "England stock test functions"
#' author: "Centre for Sustainable Energy"
#' output: pdf
#' toc: true
#' ---

#' Creates a generic crosstab function to use in the majority of tests
#' 
#' TODO: ADD WEIGHTS TO THE REPORTING TABLES
#' 
test.xtab <- function(ehs, nhm.data, ehs.value, dto.value) {
  ehs <- data.frame(aacode = ehs$aacode, weight = ehs$aagpd1314, ehs.value = ehs.value)
  nhm.data <- data.frame(aacode = nhm.data$aacode, dto.value = dto.value)
  merged <- merge(ehs, nhm.data)
  crosstab <- ddply(merged, .(ehs.value, dto.value), summarize, unweighted.cases.count = length(aacode), weighted.count = round(sum(weight),0))
  totals <- data.frame(ehs.value = "ALL CASES",
                       dto.value = "TOTAL",
                       unweighted.cases.count = sum(crosstab$unweighted.cases.count),
                       weighted.count = sum(crosstab$weighted.count))
  rbind(crosstab, totals)
}

# TEMPLATE TO USE IN THE TEST FOR GENERAL COMPARISONS OF NHM AND EHS VARIABLES

#' Produces a cross-tab comparing the EHSVARIABLE in the EHS with the NHMVARIABLE 
#' produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and XXX from the EHS
#' @param cases A dataframe containing the CSVNAME file produced from the EHS
#' @return A dataframe containing a crosstab of input XXX against output VARIABLE
#test.NAME <- function(ehs, CSV) {
#  test.xtab(
#    ehs,
#    CSV,
#    ehs$,
#    CSV$)
#}

# dwelling characteristics

#' Produces a cross-tab comparing the builtform in the EHS with the built form 
#' produced in an NHM table
#' @param ehs A dataframe containing at least aacode, C1 and C2 from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input C1+C2 against output builtform
test.builtform <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$dwtypenx,
    cases$builtform)
}

#' Produces a cross-tab comparing the buildyear in the EHS with the builtyear 
#' produced in an NHM table
#' @param ehs A dataframe containing at least aacode and C4 from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input C4 against output builtyear
test.builtyear <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    gsub("\\(([0-9]*)\\,([0-9]*)\\]"
         ,cut(ehs$fodconac, seq(1620,2020,by = 10), dig.lab = 4)
         ,replacement="\\1 - \\2"),
    gsub("\\(([0-9]*)\\,([0-9]*)\\]"
         ,cut(cases$buildyear, seq(1620,2020,by = 10), dig.lab = 4)
         ,replacement="\\1 - \\2"))
}

#' Produces a cross-tab comparing the rururb in the EHS with the NHMVARIABLE produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and rururb from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input rururb against output morphology
test.rurality <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$rumorph,
    cases$morphologytype)
}

#TESTS TO WRITE:
#numofhabitalrooms - J1
#' Produces a cross-tab comparing the numofhabitalrooms in the EHS with the number of habitable rooms produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and J1 from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input J1 against output numofhabitalrooms
test.numberhabitablerooms <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$Finrooms,
    cases$numofhabitalrooms)
}

#numofbedrooms - bedrooms
#' Produces a cross-tab comparing the numofbedrooms in the EHS with the number of habitable rooms produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and bedrooms from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input bedrooms against output numofbedrooms
test.numberbedrooms <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$NBedsX,
    cases$numofbedrooms)
}

#regiontype - la
#' Produces a cross-tab comparing the local authority in the EHS with the corresponding region produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and la from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input la against output regiontype
test.region <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$gorEHS,
    cases$regiontype)
}

# dwellings fabric information 

#wallconstruction type
# NOTE: THIS TEST IS STILL UNDER CONSTRUCTION
#' Produces a cross-tab comparing the loft insulation thickness variables in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode, loftins and M26 from the EHS
#' @param roofs A dataframe containing the roofs file produced from the EHS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.externalwallconstructiontype <- function(ehs, elevations) {
  test.xtab(
    ehs,
    elevations,
    ehs$typewstr2,
    ifelse(elevations$externalwallconstructiontype == "", "Unexposed wall"
           , levels(elevations$externalwallconstructiontype)[elevations$externalwallconstructiontype]))
}

#TODO: Improved wall insulation details test
test.cavitywallinsulation <- function(ehs, elevations) {
  externalwalltype <- ifelse(elevations$externalwallconstructiontype == "", "Unexposed wall"
                             , levels(elevations$externalwallconstructiontype)[elevations$externalwallconstructiontype])
  test.xtab(
    ehs,
    elevations,
    ehs$wallinsx,
    paste(externalwalltype, "; Cavity Wall insulation: ", elevations$cavitywallinsulation, sep = ""))
}

#loft insulation levels
#' Produces a cross-tab comparing the loft insulation thickness variables in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode, loftins and M26 from the EHS
#' @param roofs A dataframe containing the roofs file produced from the EHS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.loftinsulationthickness <- function(ehs, roofs) {
  test.xtab(
    ehs,
    roofs,
    ehs$loftinsx,
    roofs$insulationthickness)
}

#roof structure type
#' Produces a cross-tab comparing the loft insulation thickness variables in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode, loftins and M26 from the EHS
#' @param roofs A dataframe containing the roofs file produced from the EHS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.rooftype <- function(ehs, roofs) {
  test.xtab(
    ehs,
    roofs,
    ehs$typerstr,
    roofs$structuretype)
}

#roof cover type
#' Produces a cross-tab comparing the loft insulation thickness variables in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode, loftins and M26 from the EHS
#' @param roofs A dataframe containing the roofs file produced from the EHS
#' @return A dataframe containing a crosstab of input loftinS and M26 against output insulationthickness
test.roofcovertype <- function(ehs, roofs) {
  test.xtab(
    ehs,
    roofs,
    ehs$typercov,
    roofs$coveringtype)
}



#heating fuel type
#' Produces a cross-tab comparing the main space heating fuel in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and M5 from the EHS
#' @param spaceheating A dataframe containing the space-heating file produced from the EHS
#' @return A dataframe containing a crosstab of input M5 against output mainheatingfuel
test.mainheatingfuel <- function(ehs, space_heating) {
  test.xtab(
    ehs,
    space_heating,
    ehs$mainfuel,
    tolower(space_heating$mainheatingfuel))
}

#heating system
#' Produces a cross-tab comparing the main space heating system type in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and M2 from the EHS
#' @param spaceheatingsystem A dataframe containing the space-heating file produced from the EHS
#' @return A dataframe containing a crosstab of input M2 against output spaceheatingsystemtype
test.spaceheatingsystemtype <- function(ehs, space_heating) {
  test.xtab(
    ehs,
    space_heating,
    paste(ehs$fuelx, ehs$boiler, sep = " - "),
    space_heating$spaceheatingsystemtype)
}

test.spaceheatingsystemtype2 <- function(ehs, space_heating) {
  test.xtab(
    ehs,
    space_heating,
    paste(ehs$Finchtyp, ehs$Finmhboi, sep = " - "),
    space_heating$spaceheatingsystemtype)
}


# TODO: not present in main ehs data: lightingfraction (fraction - )
#' Produces a cross-tab comparing the low energy lighting fraction in the EHS with that produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and L18 from the EHS
#' @param lighting A dataframe containing the lighting file produced from the EHS
#' @return A dataframe containing a crosstab of input N1_E against output groundfloortype
# test.lighting <- function(ehs, lighting) {
#   test.xtab(
#     ehs,
#     lighting,
#     ehs$Finhtglg,
#     lighting$fraction)
# }


#groundfloor construction type
#' Produces a cross-tab comparing the ground floor construction type in the EHS with the grndfloortype produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and N1_E from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input N1_E against output groundfloortype
test.groundfloortype <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    paste("Solid floor present?", 
          ehs$livingRoomHasSolidFloor, 
          ehs$kitchenHasSolidFloor, 
          ehs$bedroomHasSolidFloor, 
          ehs$bathroomHasSolidFloor, 
          ehs$circulationHasSolidFloor, sep = "/"),
    cases$grndfloortype)
}

# socio-dems characteristics

#tenure
#' Produces a cross-tab comparing the tenure in the EHS with the tenure produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and tenure from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input tenure against output tenure
test.tenure <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$tenure8x,
    cases$tenure)
}

#adults
#' Produces a cross-tab comparing the number of adults in the EHS with the built form produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and numadults from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input numadults against output adults
test.adults <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$hhsizex - ehs$ndepchild,
    cases$adults)
}

# children
#' Produces a cross-tab comparing the number of children in the EHS with the number of children produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and numchild from the EHS
#' @param cases A dataframe containing the cases file produced from the EHS
#' @return A dataframe containing a crosstab of input numchild against output children
test.children <- function(ehs, cases) {
  test.xtab(
    ehs,
    cases,
    ehs$ndepchild,
    cases$children)
}

#income
#' Produces a cross-tab comparing the annual net income in the EHS with the household income variable produced
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and fpfullinc from the EHS
#' @param cases A dataframe containing the occupants file produced from the EHS
#' @return A dataframe containing a crosstab of input annhhinc against output householdincomebeforetax
test.income <- function(ehs, occupants) {
  test.xtab(
    ehs,
    occupants,
    cut(as.numeric(ehs$fpfullinc), seq(from = -10000, to = 1000000, by = 5000), dig.lab = 6),
    cut(occupants$householdincomebeforetax, seq(from = -10000, to = 1000000, by = 5000), dig.lab = 6))
}

#waterheating system
#' Produces a cross-tab comparing the derived hot water heating systems in the EHS 
#' with the hot water heating system type in the NHM
#' in an NHM table
#' @param ehs A dataframe containing at least aacode and watersys from the EHS
#' @param cases A dataframe containing the occupants file produced from the EHS
#' @return A dataframe containing a crosstab of input annhhinc against output householdincomebeforetax
test.waterheatingsystemtype <- function(ehs, water_heating) {
  test.xtab(
    ehs,
    water_heating,
    ehs$watersys,
    water_heating$waterheatingsystemtype)
}



