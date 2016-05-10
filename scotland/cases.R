#' ---
#' title: "Cases functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save cases data
#'
#'Create a .csv file using the function make.cases, which creates a dataframe 
#'containing a complete set of populated variables for the cases.csv stock file.
#'
#'The cases stock file contains a series of information about the dwelling, including
#'type, age, location, number of rooms, number of bedrooms, number of inhabitants, 
#'external outdoor space.
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
save.cases <- function(shcs, output) {
  write.csv(make.cases(shcs), file=output, row.names=FALSE)
}

#'Make the dataframe that contains all the information required for the cases.csv
#' file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.cases <- function(shcs) {
  #If a column in the dataframe is not taken directly from the survey the vector to
  #fill the column must first be generated.
  #Each column is created using a function:
  #For example, region.type is a function which requires the vector la from
  #the shcs dataframe (the survey data). The region.type function is defined in
  #this script.
  the.buildingtype <- building.type(shcs$C1, shcs$C2)
  the.builtformtype <- builtform.type(the.buildingtype)
  the.grndfloortype <- grndfloor.type(shcs$N1_E, shcs$C4)
  the.regiontype <- region.type(shcs$la)
  the.tenuretype <- tenure.type(shcs$tenure)
  the.loftexistence <- loft.existence(shcs$M28, shcs$C1, shcs$C2)
  the.draftlobbyexistence  <- draftlobby.existence(the.builtformtype)
  the.morphologytype <- morphology.type(shcs$rururb)
  the.livingareafaction <- living.areafaction(shcs$J1)
  the.frontplotdepth <- front.plotdepth(the.morphologytype,the.builtformtype)
  the.frontplotwidth <- front.plotwidth(the.morphologytype,the.builtformtype)
  the.backplotdepth <- back.plotdepth(the.morphologytype,the.builtformtype)
  the.backplotwidth <- back.plotwidth(the.morphologytype,the.builtformtype)
  the.hasaccesstooutsidespace <- has.accesstooutsidespace(the.morphologytype,
                                                          the.builtformtype)
  the.ongasgrid <- on.gasgrid(shcs$L1,shcs$M5)
  the.partlyownsroof <- partly.ownsroof(shcs$uprn_new)
  #creates a dataframe containing column names (word before the = sign) and a vector
  #to fill that column (vector after the = sign)
  data.frame(aacode = shcs$uprn_new
             ,adults = shcs$numadult
             ,backplotdepth = the.backplotdepth
             ,backplotwidth = the.backplotwidth
             ,buildyear = shcs$C4
             ,builtformtype = the.builtformtype
             ,children = shcs$numchild
             ,dwellingcaseweight = shcs$laWghtP
             ,grndfloortype = the.grndfloortype
             ,frontplotdepth = the.frontplotdepth
             ,frontplotwidth = the.frontplotwidth
             ,hasaccesstooutsidespace = the.hasaccesstooutsidespace
             ,hasdraftlobby = the.draftlobbyexistence               
             ,hasloft = the.loftexistence
             ,householdcaseweight = shcs$laWghtP
             ,livingareafaction = the.livingareafaction
             ,morphologytype = the.morphologytype
             ,numofhabitalrooms = shcs$J1
             ,numofbedrooms = shcs$bedrooms           
             ,ongasgrid = the.ongasgrid
             ,partlyownsroof = the.partlyownsroof
             ,regiontype = the.regiontype
             ,tenuretype = the.tenuretype
             )
}

#' \pagebreak
#' 
#'##Region
#' 
#'Scottish local authority (la field in shcs) to NHM region type
#' 
#'@param la - la column from SHCS
region.type <- function(la) {
    as.factor(checked.revalue(
        la,
        c("Aberdeen City" = "easternscotland",
          "Aberdeenshire" = "easternscotland",
          "Angus" = "easternscotland",
          "Argyll and Bute" = "westernscotland",
          "Clackmannanshire" = "easternscotland",
          "Dumfries and Galloway" = "westernscotland",
          "Dundee City" = "easternscotland",
          "East Ayrshire" = "westernscotland",
          "East Dunbartonshire" = "westernscotland",
          "East Lothian" = "easternscotland",
          "East Renfrewshire" = "westernscotland",
          "City of Edinburgh" = "easternscotland",
          "Eilean Siar" = "westernscotland",
          "Falkirk" = "westernscotland",
          "Fife" = "easternscotland",
          "Glasgow City" = "westernscotland",
          "Highland" = "northernscotland",
          "Inverclyde" = "westernscotland",
          "Midlothian" = "easternscotland",
          "Moray" = "easternscotland",
          "North Ayrshire" = "westernscotland",
          "North Lanarkshire" = "westernscotland",
          "Orkney Islands" = "northernscotland",
          "Perth and Kinross" = "easternscotland",
          "Renfrewshire" = "westernscotland",
          "Scottish Borders" = "easternscotland",
          "Shetland Islands" = "northernscotland",
          "South Ayrshire" = "westernscotland",
          "South Lanarkshire" = "westernscotland",
          "Stirling" = "westernscotland",
          "West Dunbartonshire" = "westernscotland",
          "West Lothian" = "easternscotland")))
}

#' 
#'Create morphologytype
#' 
#'There is only a 2-fold definition of morphology type in the scottish survey.
#' This has been compared to the english 4-fold definition used in the NHM to
#' determine which values to map against
#' 
#'@param morphology - rururb from scottish survey
morphology.type <- function(morphology){
  as.factor(checked.revalue(
    morphology,
    c("Urban"="urban"
      ,"Rural"="townandfringe"))) 
}

#'
#' \pagebreak
#' 
#'##Tenure
#' 
#'Generate tenure type
#' 
#'@param tenure - tenure type column from scottish survey
tenure.type <- function(tenure) {
    as.factor(checked.revalue(
        tenure,
        c("owner-occupier" = "owneroccupied",
          "private-rented" = "privaterented",
          "LA/other public" = "localauthority",
          "HA/co-op" = "housingassociation",
          "Unobtainable" = "owneroccupied")))
}

#' \pagebreak
#' 
#'##Building type
#' 
#'Convert a building type (combination of C1 and C2) into a built form type
#' 
#'@param buildingtype - see common.R building.type
builtform.type <- function(buildingtype) {
  as.factor(checked.revalue(
    buildingtype,c(
      "Corner / enclosed end" = "endterrace",
      "Detached" = "detached",
      "Enclosed mid" = "midterrace",
      "Mid-terrace" = "midterrace",
      "Mid-terrace with passage" = "midterrace",
      "End terrace" = "endterrace",
      "Semi-detached" = "semidetached",
      "4-in-a-block" = "purposebuiltlowriseflat",
      "Flat from conversion" = "convertedflat",
      "Tenement" = "purposebuiltlowriseflat",
      "Tower or slab" = "purposebuilthighriseflat")))
}

#' \pagebreak
#' 
#'##Building construction
#' 
#'Map N1E to ground floor type
#' 
#'@param N1E - N1_E ground/lowest level floor type column from Scottish survey
grndfloor.type <- function(N1E, C4) {
  floor <- as.factor(checked.revalue(
                      N1E,
                      c("Susp timber" = "suspendedtimber",
                        "Susp not timber" = "solid",
                        "Solid" = "solid",
                        "Not applicable" = "solid",
                        "Unobtainable" = "solid")))
  
  
  floor <- ifelse(C4 <= 1929 & N1E == "Unobtainable", "suspendedtimber",
                  ifelse(C4 <= 1929 & N1E == "Not applicable", "suspendedtimber",
                         levels(floor)[floor]))
  
  return(floor)
  
}

#'Create existence of loft
#' 
#'@param loft - M28 is Loft hatch present from scottish survey
#'There is not specific variable identifying the presence of a loft, so the presence
#'of a loft hatch is used as a proxy for identifying the presence of a loft
loft.existence <- function(loft, C1, C2){
  loftins <- as.factor(checked.revalue(
                        loft,
                        c("No" = "FALSE",
                          "yes, not draughtproofed" = "TRUE",
                          "yes, draughtproofed" = "TRUE",
                          "Eaves door" = "TRUE",
                          "Not applicable/no loft" = "FALSE",
                          "Unobtainable" = "FALSE")))
  
  builtformtype <- building.type(C1, C2)
  
  is.flat <- is.flat(builtformtype)
  is.house <- is.house(builtformtype)
  
  loftins <- ifelse(is.house & loft == "Not applicable/no loft", "FALSE", 
                    ifelse(is.house & loft != "Not applicable/no loft", "TRUE",
                           levels(loftins)[loftins]))
  return(loftins)

}

#'Create existence of draught lobby
#' 
#'This uses the rules detailed in the RD-SAP methodology (SAP 9.91 Appendix S) that 
#'specifies that if the building type is a flat (i.e. low-rise, high-rise or 
#'converted) there is a draught lobby present, whereas if the building type is not a 
#'flat (i.e. a house) then there is no draught lobby present
#' 
#'@param draftlobby - thebuiltformtype created through building.type
draftlobby.existence <- function(draftlobby){
  as.factor(checked.revalue(
    draftlobby,
    c("purposebuiltlowriseflat" = "TRUE",
      "endterrace" = "FALSE",
      "detached"  = "FALSE",
      "midterrace" = "FALSE",
      "convertedflat" = "TRUE",
      "semidetached" = "FALSE",
      "purposebuilthighriseflat" = "TRUE")))
}


#'Create partlyownsroof
#' 
#'Assumed all records to have 'FALSE' values as there is no information available 
#' about this in the scottish survey
#' 
#'@param partroof - from uprn_new ensuring that the vector created is the correct
#' length
partly.ownsroof <- function(partroof){
  partroof <- "FALSE"
}

#' \pagebreak
#' 
#' ##Gas grid
#' 
#'Create ongasgrid
#' 
#'This is created from the description of which mains services are available for
#' each case (electric and or gas). However if the main heating fuel is mains(gas)
#' the case is also forced to be true for ongasgrid.
#' 
#'@param services - from L1 (What mains services?)
#' 
#'@param heatingfuel - from space heating, used to overide cases where the main
#' heating system is mains gas, but survey suggests off the gas grid
on.gasgrid <- function(services,heatingfuel){
  services <- as.factor(checked.revalue(
    services,
    c("Electricity only" = "FALSE"
      ,"Electricity and gas" = "TRUE"
      ,"Gas only" = "TRUE"
      ,"No services" = "FALSE"
      ,"Unobtainable" = "FALSE"
    )))
  services[heatingfuel=="Gas (mains)"] <- "TRUE"
  return(services)
}


#' \pagebreak
#' 
#'##Living area fraction
#' 
#'Create livingareafaction
#' 
#' No information is available for the dimensions of the living space. The only 
#' information available to calculate the living area fraction is the number of 
#' rooms, which can be used in conjunction with RD-SAP Table S16 in section S9.2,  
#' which specifies the living area fraction to used depending on the total number of 
#' habitable rooms in a dwelling. Table S16 is recreated and used below.
#' 
#'@param livingarea -  number of rooms(J1) from the scottish survey
living.areafaction <- function(livingarea){
  #Cases with more than 14 rooms are converted to having 14 rooms (less than 10
  #cases). This does not include the values that indicate unobtainable (88) or
  #unknown (99)
  livingarea[livingarea >= 14 & livingarea != 88 & livingarea != 99] <- 14 
  #The number of roomms are mapped against living area fractions.
  #Unobtainable and unknown are mapped to the same value as for cases with 5 rooms
  #as the average number of rooms per house in the scottish survey was 5 rooms.
  livingarea <- checked.renum(livingarea,
                            data.frame(a = c(   1,   2,   3,   4,   5,
                                                6,   7,   8,   9,  10,
                                                11,  12,  13,  14, 88,
                                                99)
                                     , b = c( 0.75,0.50,0.30,0.25,0.21,
                                              0.18,0.16,0.14,0.13,0.12,
                                              0.11,0.10,0.10,0.09,0.21,
                                              0.21)))
}

#' \pagebreak
#' 
#'##Outside space (plots)
#' 
#'Create frontplotdepth
#' 
#'No available information in the Scottish survey: frontplotdepth,frontplotwidth,
#' backplotdepth, backplotwidth
#' 
#'These can be used in the NHM when installing such measures as ground source head
#' pumps and biomass biomass whereby these can only be installed where properties 
#' have sufficient outdoor space to install ground coils or a fuel store.
#'  
#'Therefore it has been assumed that in all cases frontplotwidth and frontplotdepth
#' is 0m
#'For cases that are not flats and not urban it has been assumed that backplotdepth
#' is 5m and backplotwidth is 10m, thereby creating an external area of 50m2.
#'  
#'These assumptions are consistant with previous stock creations for Scotland. 
#'However, these values can be changed below as required.
#' 
#'@param morphology - the.morphologytype calculated in cases.R
#' 
#'@param builtform - the.builtformtype calculated in cases.R
front.plotdepth <- function(morphology,builtform){
  #A data frame is created with morphology and builtform. Then an additional column
  #called plotdepth is created and populated with 0. This column is then returned by
  #the function
  frontdepth <- data.frame(morphology,builtform)
  frontdepth$plotdepth <- 0
  return(frontdepth$plotdepth)
}

#'Create frontplotwidth
#' 
#'see frontplot depth
#' 
#'@param morphology - the.morphologytype calculated in cases.R
#' 
#'@param builtform - the.builtformtype calculated in cases.R
front.plotwidth <- function(morphology,builtform){
  frontwidth <- data.frame(morphology,builtform)
  frontwidth$plotwidth <- 0
  return(frontwidth$plotwidth)
}

#'Create backplotdepth
#' 
#'see frontplot depth
#' 
#'@param morphology - the.morphologytype calculated in cases.R
#' 
#'@param builtform - the.builtformtype calculated in cases.R
back.plotdepth <- function(morphology,builtform){
  #A data frame is created with morphology and builtform. Then an additional column
  #called plotdepth is created and populated with 0.For cases where the morphology is
  #not urban or any flats then this is updated to 5.This column is then returned by
  #the function
  backdepth <- data.frame(morphology,builtform)
  backdepth$plotdepth <- 0
  backdepth <- within(backdepth,
                       plotdepth[morphology != "urban" &
                                   (builtform != "purposebuiltlowriseflat" &
                                   builtform != "purposebuilthighriseflat" &
                                   builtform != "convertedflat")] <- 5)
  return(backdepth$plotdepth)
}


#'Create backplotwidth
#' 
#'see frontplot depth
#' 
#'@param morphology - the.morphologytype calculated in cases.R
#' 
#'@param builtform - the.builtformtype calculated in cases.R
back.plotwidth <- function(morphology,builtform){
  backwidth <- data.frame(morphology,builtform)
  backwidth$plotwidth <- 0
  backwidth <- within(backwidth,
                      plotwidth[morphology != "urban" &
                                  (builtform != "purposebuiltlowriseflat" &
                                     builtform != "purposebuilthighriseflat" &
                                     builtform != "convertedflat")] <- 10)
  return(backwidth$plotwidth)
}

#'Create hasaccesstooutsidespace
#' 
#'This is a flag based on the plotdepths and plotwidths. It is made using the same
#' logic that was used to create back.plotdepth as any cases where this is true will
#' also have access to outside space.
#' 
#' UPDATE 27.08.2015:
#' In the absence of any data, default assumption that dwellings/households have 
#' access to outdoor space
#' 
#'@param morphology - the.morphologytype calculated in cases.R
#' 
#'@param builtform - the.builtformtype calculated in cases.R
has.accesstooutsidespace <- function(morphology,builtform){
  outside <- data.frame(morphology,builtform)
  outside$space <- "TRUE"
  #outside <- within(outside,
  #                 space[morphology != "urban" &
  #                                (builtform != "purposebuiltlowriseflat" &
  #                                   builtform != "purposebuilthighriseflat" &
  #                                   builtform != "convertedflat")] <- "TRUE")
  return(outside$space)
}

