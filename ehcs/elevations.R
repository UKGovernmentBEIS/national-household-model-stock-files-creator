#' ---
#' title: "Elevation functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make elevations data
#'
#'Create a .csv file using the function make.elevations, which creates a dataframe 
#'containing a complete set of populated variables for the elevations.csv stock 
#'file.
#'
#'The elevations file contains four rows for each dwelling case, one for each 
#'elevation of a dwelling (front, back, left and right). The information in the 
#'survey is used to determine which of these elevations are attached to party walls, 
#'the construction types of the unattached walls and levels of insulation on each 
#'elevation. Also included in data on types of window (glazing and frame type) and 
#'what proportion of each elevation is windows, and how many and the type of doors 
#'present in each elevation.
#'
#'Each elevation can have only one wall type and can only have full or no insulation 
#'but multiple types of insulation are allowed (i.e. an external wall on an elevation
#'could have both external and internal wall insulation present).
#'
#' Attachments and proportion of windows are expressed in tenths. 
#'
#' @param allEntries - combined sav files data.frame from the EHS data
elevations.make <- function(allEntries, doorEntries) {
  allElevations <- build.empty.elevations(allEntries)
  
  elevationDetail <- calc.elevation.detail(allEntries)
  wallConstruction <- build.external.walls(allEntries)
  doors <- build.doors(doorEntries)
  
  allElevations <- Reduce(function (a,b) {
    merge(a,b, by = c("aacode","elevationtype"), all.x = TRUE)
  }, list(allElevations, elevationDetail, wallConstruction, doors))
  
  #Fix attached/party walls
  #if walls are 100% attached then they should have no external wall type
  fixedWallConstructionType <- data.table(
    aacode = allElevations$aacode
    ,elevationtype = allElevations$elevationtype
    ,externalwallconstructiontype = ifelse(allElevations$tenthsattached < 10, 
                                    allElevations$externalwallconstructiontype, NA)
    ,cavitywallinsulation = ifelse(allElevations$tenthsattached < 10, 
                                   allElevations$cavitywallinsulation, FALSE)
    ,externalwallinsulation = ifelse(allElevations$tenthsattached < 10, 
                                     allElevations$externalwallinsulation, FALSE)
  )
  
  allElevations <- subset(allElevations, select = c(-externalwallconstructiontype,
                                    -cavitywallinsulation, -externalwallinsulation))
  allElevations <- merge(allElevations, fixedWallConstructionType, 
                         by = c("aacode","elevationtype"))
  
  #Put window data onto every elevation
  allElevations <- merge(allElevations, build.windows(allEntries), 
                         by = "aacode", all.x = TRUE)
  
  print(paste("elevations DTO complete; number of records: ", nrow(allElevations)))
  
  return(allElevations)
}

#' ##Window glazing
#'
#'Determines the window glazing proportion in the dwelling and constructs a window
#'data.frame
build.windows <- function(allEntries){
  
  percentageDblGlazed <- as.numeric(levels(
    percentage.glazed(allEntries$dblglaz4))[percentage.glazed(allEntries$dblglaz4)])
  predominantWindowType <- as.character(levels(
    window.frame.type(allEntries$typewin))[window.frame.type(allEntries$typewin)])
  
  windows <- data.frame(
    aacode = allEntries$aacode,
    percentagedoubleglazed = percentageDblGlazed,
    doubleglazedwindowframe = ifelse(percentageDblGlazed > 0,  
                                                      predominantWindowType, "NULL"),
    singleglazedwindowframe = ifelse(percentageDblGlazed < 100,  
                                                      predominantWindowType, "NULL")
  )
  
  return (windows)
}

#' ##Window frame
#' 
#' Converts the SPSS variable for window frame type into a value the NHM import 
#' understands
#'
#'@assumption SPSS value of "Mixed types" defaults to Wood
#'
#'@param typewin
window.frame.type <- function(typewin){
  as.factor(checked.revalue(
    typewin,c(
      "double-glazed- metal" =  "Metal",
      "double-glazed- UPVC" = "uPVC",
      "double-glazed- wood" = "Wood",
      "single-glazed- metal" = "Metal",
      "single-glazed- UPVC" = "uPVC",
      "single-glazed- wood sash" = "Wood",
      "single-glazed- wood casement" = "Wood",
      "mixed types" = "Wood"
    )))}

percentage.glazed <- function(dblglaz4){
  as.factor(checked.revalue(
    dblglaz4,c(
      "no double glazing" =  0,
      "less than half" = 25,
      "more than half" = 75,
      "entire house" = 100
    )))}

#' ##Doors
#' 
#' Builds doors in front and back elevations, for left and right elevations doors of 
#' all types are always zero as we have no information on these from the survey.
build.doors <- function(doorEntries){
  # Summaries the number of front doors of different frame types
  frontDoors_summmary <- cast(doorEntries, aacode ~ type, value = "Fexdf1no")
  
  frontElevations <- data.frame(
    aacode = frontDoors_summmary$aacode,
    elevationtype = rep("FRONT", length(frontDoors_summmary$aacode)),
    "doorframe:wood,doortype:solid" = frontDoors_summmary$Wood,
    "doorframe:wood,doortype:glazed" = rep(0, length(frontDoors_summmary$aacode)),  
    "doorframe:metal,doortype:solid" =  frontDoors_summmary$Metal,
    "doorframe:metal,doortype:glazed" = rep(0, length(frontDoors_summmary$aacode)),	
    "doorframe:upvc,doortype:solid" = frontDoors_summmary$UPVC,	
    "doorframe:upvc,doortype:glazed"= rep(0, length(frontDoors_summmary$aacode)),
    check.names = FALSE
  )
  
  # Summaries the number of back doors of different frame types
  backDoors_summmary <- cast(doorEntries, aacode ~ type, value = "Fexdf2no")
  
  backElevations <- data.frame(
    aacode = backDoors_summmary$aacode,
    elevationtype = rep("BACK", length(backDoors_summmary$aacode)),
    "doorframe:wood,doortype:solid" = backDoors_summmary$Wood,
    "doorframe:wood,doortype:glazed" = rep(0, length(backDoors_summmary$aacode)),  
    "doorframe:metal,doortype:solid" = backDoors_summmary$Metal,
    "doorframe:metal,doortype:glazed" = rep(0, length(backDoors_summmary$aacode)),	
    "doorframe:upvc,doortype:solid" = backDoors_summmary$UPVC,	
    "doorframe:upvc,doortype:glazed"= rep(0, length(backDoors_summmary$aacode)),
    check.names = FALSE
  )
  
# Note not a typo, we have no data for left and right elevations so just add zeros 
# and use the length of the back_door summary for the number of rows to populate
  leftElevations <- data.frame(
    aacode = backDoors_summmary$aacode,
    elevationtype = rep("LEFT", length(backDoors_summmary$aacode)),
    "doorframe:wood,doortype:solid" = rep(0, length(backDoors_summmary$aacode)),
    "doorframe:wood,doortype:glazed" = rep(0, length(backDoors_summmary$aacode)),  
    "doorframe:metal,doortype:solid" = rep(0, length(backDoors_summmary$aacode)),
    "doorframe:metal,doortype:glazed" = rep(0, length(backDoors_summmary$aacode)),  
    "doorframe:upvc,doortype:solid" = rep(0, length(backDoors_summmary$aacode)),	
    "doorframe:upvc,doortype:glazed"= rep(0, length(backDoors_summmary$aacode)),
    check.names = FALSE
  )
    
  rightElevations <- data.frame(
    aacode = backDoors_summmary$aacode,
    elevationtype = rep("RIGHT", length(backDoors_summmary$aacode)),
    "doorframe:wood,doortype:solid" = rep(0, length(backDoors_summmary$aacode)),
    "doorframe:wood,doortype:glazed" = rep(0, length(backDoors_summmary$aacode)),  
    "doorframe:metal,doortype:solid" = rep(0, length(backDoors_summmary$aacode)),
    "doorframe:metal,doortype:glazed" = rep(0, length(backDoors_summmary$aacode)),  
    "doorframe:upvc,doortype:solid" = rep(0, length(backDoors_summmary$aacode)),  
    "doorframe:upvc,doortype:glazed"= rep(0, length(backDoors_summmary$aacode)),
    check.names = FALSE
  )
  
  
  allDoors <- rbind(frontElevations, backElevations, leftElevations, rightElevations)
  return(allDoors)
}

#' ##External wall construction type and insulation
#' 
#' Builds external walls and insulation, assumes all walls have the same construction
#'  type, derived from typewstr2
#'
#' @assumption internalinsulation - Always FALSE, is dealt with later on 
#'
#' @param Felcavff - cavitywallinsulation(Yes or No)
#' @param Felextff - externalwallinsulation(Yes or No)
#' @param typewstr2 - predominant wall construction type
build.external.walls <- function(allEntries) {
  
  wallConstructionType <- wall.structure.type(allEntries$typewstr2)
  
  can.have.cwi <- wallConstructionType %in% c("Cavity", "SystemBuild", "TimberFrame")
  
  frontElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("FRONT", length(allEntries$aacode)),
    externalwallconstructiontype = wallConstructionType,
    cavitywallinsulation = is.na(allEntries$Felcavff) == FALSE & 
                                          allEntries$Felcavff == "Yes" & can.have.cwi,
    internalinsulation = rep(FALSE, length(allEntries$aacode)),
    externalwallinsulation = is.na(allEntries$Felextff) == FALSE & 
                                                          allEntries$Felextff == "Yes"
  )
  
  backElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("BACK", length(allEntries$aacode)),
    externalwallconstructiontype = wallConstructionType,
    cavitywallinsulation = is.na(allEntries$Felcavbf) == FALSE & 
                                        allEntries$Felcavbf == "Yes" & can.have.cwi,
    internalinsulation = rep(FALSE, length(allEntries$aacode)),
    externalwallinsulation = is.na(allEntries$Felextbf) == FALSE & 
                                                          allEntries$Felextbf == "Yes"
  )

  leftElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("LEFT", length(allEntries$aacode)),
    externalwallconstructiontype = wallConstructionType,
    cavitywallinsulation = is.na(allEntries$Felcavlf) == FALSE & 
                                          allEntries$Felcavlf == "Yes" & can.have.cwi,
    internalinsulation = rep(FALSE, length(allEntries$aacode)),
    externalwallinsulation = is.na(allEntries$Felextlf) == FALSE & 
                                                          allEntries$Felextlf == "Yes"
  )

  rightElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("RIGHT", length(allEntries$aacode)),
    externalwallconstructiontype = wallConstructionType,
    cavitywallinsulation = is.na(allEntries$Felcavrf) == FALSE 
                                        & allEntries$Felcavrf == "Yes" & can.have.cwi,
    internalinsulation = rep(FALSE, length(allEntries$aacode)),
    externalwallinsulation = is.na(allEntries$Felextrf) == 
                                                  FALSE & allEntries$Felextrf == "Yes"
  )

  mergedElevations <- rbind(frontElevations, backElevations, leftElevations, rightElevations)
  mergedElevations$externalwallconstructiontype <- 
                          as.character(mergedElevations$externalwallconstructiontype)
  
  return(mergedElevations)
}

#' ## Door frame type
#' 
#' Determines the door frame type from the doors sav file in the EHS.
doors.frame.type <- function(type){
  as.factor(checked.revalue(
    type,c(
      "Wood" = "Wood",
      "UPVC" = "uPVC",
      "Metal" = "Metal")))}

wall.structure.type <- function(typewstr2){
    return (as.character(
      levels(wall.structure.type.lookup(typewstr2))[wall.structure.type.lookup(typewstr2)]))
}


#'## Predominant wall structure type
#'
#' @param typewstr2 - Predominant wall structure type
#' @assumption Mixed Types are assumed to be GraniteOrWhinstone - Sure we can do better
wall.structure.type.lookup <-function(typewstr2){
  as.factor(checked.revalue(
    typewstr2,c(
      "mixed types" = "GraniteOrWhinstone",
      "masonry cavity" = "Cavity",
      "masonry single leaf" = "SolidBrick",
      "9 inch solid" = "GraniteOrWhinstone",
      "greater than 9 inch solid" = "Sandstone",
      "in situ concrete" = "SystemBuild",
      "concrete panels" = "SystemBuild",
      "timber/metal/plastic panels" = "TimberFrame")))}

#' ## Wall attachments
#' 
#' Calculates tenths attached, opening and partywall for each elevation
#'
#' @param aacode
#' @param Felfenfw - tenths opening front wall
#' @param Felfenbw - tenths opening back wall
#' @param Felfenlw - tenths opening left wall
#' @param Felfenrw - tenths opening right wall
#'
#' @note - There are 113 Case where a house has an NA value for back tenths attached
#' @note - There are 3947 Case where a house has an NA value for left tenths attached
#' @note - There are 4015 Case where a house has an NA value for right tenths attached
#' 
#' @assumption - If tenths attached for an elevation in house or flat is NA set to 10
#' @assumption - If tenths opening for an elevation or house is NA set to 0
calc.elevation.detail <- function(allEntries){
  
  isAHouse <- is.a.house(allEntries$dwtype8x)
  
  frontTenthsAttached <- ifelse(is.a.house(allEntries$dwtype8x) == TRUE, 
                         ifelse(is.na(allEntries$Fvwtenff), 10, allEntries$Fvwtenff),
                         10 - allEntries$Fdffrooa)
  backTenthsAttached <- ifelse(is.a.house(allEntries$dwtype8x) == TRUE, 
                        ifelse(is.na(allEntries$Fvwtenbf),10, allEntries$Fvwtenbf),
                        10 - allEntries$Fdfbckoa)
  leftTenthsAttached <- ifelse(is.a.house(allEntries$dwtype8x) == TRUE, 
                        ifelse(is.na(allEntries$Fvwtenlf),10, allEntries$Fvwtenlf),
                        10 - allEntries$Fdfrigoa)
  rightTenthsAttached <- ifelse(is.a.house(allEntries$dwtype8x) == TRUE, 
                         ifelse(is.na(allEntries$Fvwtenrf),10, allEntries$Fvwtenrf), 
                         10 - allEntries$Fdffrooa)
  
  frontElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("FRONT", length(allEntries$aacode)),
    tenthsattached = frontTenthsAttached,
    tenthsopening = ifelse(is.na(allEntries$Felfenfw), 0, allEntries$Felfenfw),
    tenthspartywall = ifelse(isAHouse == TRUE, frontTenthsAttached, allEntries$Fdffroof)
  )
  
  backElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("BACK", length(allEntries$aacode)),
    tenthsattached = backTenthsAttached,
    tenthsopening = ifelse(is.na(allEntries$Felfenbw), 0, allEntries$Felfenbw),
    tenthspartywall = ifelse(isAHouse == TRUE, backTenthsAttached, allEntries$Fdfbckof)
  )
  
  leftElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("LEFT", length(allEntries$aacode)),
    tenthsattached = leftTenthsAttached,
    tenthsopening = ifelse(is.na(allEntries$Felfenlw), 0, allEntries$Felfenlw),
    tenthspartywall = ifelse(isAHouse == TRUE, leftTenthsAttached, allEntries$Fdflftof)
  )
  
  rightElevations <- data.frame(
    aacode = allEntries$aacode,
    elevationtype = rep("RIGHT", length(allEntries$aacode)),
    tenthsattached = rightTenthsAttached,
    tenthsopening = ifelse(is.na(allEntries$Felfenrw), 0, allEntries$Felfenrw),
    tenthspartywall = ifelse(isAHouse == TRUE, rightTenthsAttached, allEntries$Fdfrigof)
  )
  
  mergedElevations <- rbind(frontElevations, backElevations, leftElevations, rightElevations)
  
  return(mergedElevations)
}

#' ##Build elevations
#' 
#' Constructs the basic structure of the elevations file
build.empty.elevations <- function(allEntries){
  elevations <- data.frame(elevationtype = c("FRONT","BACK","LEFT","RIGHT"))
  allElevations <- data.frame(aacode = allEntries$aacode)
  allElevations <- merge(allElevations, elevations)
  
  return(allElevations)
}
