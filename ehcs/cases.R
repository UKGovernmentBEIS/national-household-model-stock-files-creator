#' ---
#' title: "Cases functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---
#' 

#+ echo=FALSE, warnings=FALSE
source("common.R", chdir=T)

#'##Make cases data
#'
#'Create a .csv file using the function make.cases, which creates a dataframe 
#'containing a complete set of populated variables for the cases.csv stock file.
#'
#'The cases stock file contains a series of information about the dwelling, including
#'type, age, location, number of rooms, number of bedrooms, number of inhabitants, 
#'external outdoor space.
#' 
#' @param allEntries - This is a dataframe containing a a data-frame derived from one
#'  or more survey spss(sav) files and their variables. Survey files which contain 
#'  more than one entry per house case are handled seperatley and should be specified
#'  in the following parameters.
#'  
#' @assumption - If not a house then assume it has a draught lobby
#' 
#' @param allEntries - combined sav files from the EHS
cases.make <- function(allEntries) {
  # Create a variable which contains a count of all survey case
  numOfCases = length(allEntries$aacode)
  
  # Calculates the dimensions for a house case, see  calc.plot.dimensions function 
  # for more detail, where allEntries$[variable name] is a merged spss data.frame and 
  # variable name is the name from ths spss files
  plot.dimensions <- calc.plot.dimensions(
      widthOfPlot = allEntries$Fexwidth,
      doesFrontPlotExist = allEntries$Fexplotf,
      doesBackPlotExist = allEntries$Fexplotr,
      depthOfBackPlot = allEntries$Fexp2fdp, 
      depthOfFrontPlot =  allEntries$Fexp1fdp)
    
  occupants <- occupant.counts(allEntries$aacode, allEntries$hhsizex, 
                               allEntries$ndepchild)
  
  living.area.data <- cal.livingarea.data(allEntries$Finrooms,allEntries$Finlivwi,
                                          allEntries$Finlivde,allEntries$FloorArea.y)
  
  # Construct a data-frame for each house case
  cases <- data.frame(
            aacode = allEntries$aacode,
            adults = occupants$adults,
            backplotdepth = plot.dimensions$backplotDepth,
            backplotwidth = plot.dimensions$backplotWidth,
            buildyear = allEntries$fodconac,
            builtformtype = builtform.type(allEntries$dwtypenx),
            children = occupants$children,
            dwellingcaseweight = allEntries$aagpd1314,
            grndfloortype = groundfloor.construction.type(allEntries$kitchenHasSolidFloor, 
                                                allEntries$livingRoomHasSolidFloor),
            frontplotdepth = plot.dimensions$frontplotDepth,
            frontplotwidth = plot.dimensions$frontplotDepth,
            hasaccesstooutsidespace = has.access.to.outside.space(allEntries$Fexpltyp,
                                                    plot.dimensions$backplotArea, 
                                                    plot.dimensions$frontplotArea),
            hasdraftlobby = (is.a.house(allEntries$dwtype8x) == FALSE),
            hasloft = has.loft(allEntries$Flitypes, allEntries$Flithick),
            householdcaseweight = household.weight(allEntries$aagph1314),
            livingareafaction = living.area.data$livingAreaFaction,
            morphologytype = morphology.type.lookup(allEntries$rumorph),
            numofhabitalrooms = living.area.data$numHabitalRooms,
            numofbedrooms = count.num.of.bedrooms(allEntries),
            ongasgrid = ifelse(is.na(allEntries$Fingasms), FALSE, 
                               ifelse(allEntries$Fingasms == "Yes", TRUE, FALSE)),
            partlyownsroof = ifelse(is.na(allEntries$Owntype), FALSE, 
                                    owns.part.of.roof(allEntries$Owntype)),
            regiontype = region.type(allEntries$gorEHS),
            tenuretype = tenure.type(allEntries$tenure8x))
  
  print(paste("Cases DTO complete; number of records: ", nrow(cases)))
  return(cases)
  
}


#' ## Rurality
#' 
#' Determine the rurality of each dwelling by recoding an existing variable
#' 
morphology.type.lookup <- function(rumorph){
  as.factor(checked.revalue(
    rumorph,c(
      "hamlets and isolated dwellings" = "hamletsandisolateddwellings"
      ,"village" = "village"
      ,"town and fringe"= "townandfringe"
      ,"urban > 10k" = "urban"
      )))
}


#' ## Presence of a loft
#' 
#' If Loft type is not NA or not loft OR insulation thickness greate than 0
#' then has a loft
#'
#' @param FLITYPES - Services/Loftype
#' @param FLITHICK - Approx loft insulation thickness
has.loft <- function(FLITYPES, FLITHICK) {
  hasNoLoft <- is.na(FLITYPES) | FLITYPES == "No loft - flat or very shallow pitch"
  noInsulation <- is.na(FLITHICK) | FLITHICK == "Don t know thickness" | FLITHICK == 
                    "No insulation" | FLITHICK == 0
  
  return(hasNoLoft == FALSE | noInsulation == FALSE)
}

#' ##Tenure
#' 
#' Determine the tenure of the dwelling by recoding an existing variable

tenure.type <- function(tenure8x){
  as.factor(checked.revalue(
    tenure8x,c(
      "owner occupied - occupied" = "OwnerOccupied",
      "private rented - occupied" = "PrivateRented",
      "local Authority - occupied" = "LocalAuthority",
      "RSL - occupied" = "HousingAssociation",
      "owner occupied - vacant" = "OwnerOccupied",
      "private rented - vacant" = "PrivateRented",
      "local Authority - vacant" = "LocalAuthority",
      "RSL - vacant" = "HousingAssociation"
      )))
}

region.type <- function(gorEHS){
  as.factor(checked.revalue(
    gorEHS,c(
      "North West" = "northwest",
      "North East" = "northeast",
      "Yorkshire and the Humber" = "yorkshireandhumber",
      "East Midlands" = "eastmidlands",
      "West Midlands" = "westmidlands",
      "East" = "eastofengland",
      "London" = "london",
      "South East" = "southeast",
      "South West" = "southwest")))
}

#' ## Number of bedrooms
#'
#' Determined from the introoms file, which has a main bedroom variable and then a 
#' uses the addition room variables to determine which of them are additional 
#' bedrooms
count.num.of.bedrooms <- function(allEntries){
  numOfBedrooms = 0;
  
  numOfBedrooms <- ifelse(allEntries$Finbedex == "Yes", numOfBedrooms + 1, 0)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex1ex, allEntries$Finex1fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex2ex, allEntries$Finex2fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex3ex, allEntries$Finex3fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex4ex, allEntries$Finex4fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex5ex, allEntries$Finex5fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex6ex, allEntries$Finex6fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  numOfBedrooms <- ifelse(is.a.bedroom(allEntries$Finex7ex, allEntries$Finex7fu), 
                          numOfBedrooms + 1, numOfBedrooms)
  
  return (numOfBedrooms)
}

#' Specific function to determine whether the addition rooms are bedrooms
#' 
#' @param room.function
is.a.bedroom <- function(does.room.exist, room.function){
  isBedroom <- ifelse(does.room.exist == "Yes", TRUE, FALSE)
  isBedroom <- ifelse(isBedroom, is.na(room.function) == FALSE, FALSE)
  isBedroom <- ifelse(isBedroom, ifelse(room.function == "Twin bedroom" | 
                            room.function == "Single bedroom", TRUE, FALSE), FALSE)
  
  return(isBedroom)
}

#' ## Household weight
#' 
#' Returns the dwelling weight, if this is NA the function returns -9
#'
#'@param aagph1314 - DwellWeight_PairedCases
household.weight <- function(aagph1314){
  return (ifelse(is.na(aagph1314), -9, aagph1314))
}


#' ## Access to outside space
#' 
#' Returns true only if type of plot is not No private plot or shared plot and
#' either front or back plots have an area greated than 0.
#'
#'@param Fexpltyp type of plot
#'@param backPlotArea
#'@param frontPlotArea
has.access.to.outside.space <- function(Fexpltyp, backPlotArea, frontPlotArea){
  okPlotType <- !Fexpltyp == "No private plot or shared plot"
  return(okPlotType & (backPlotArea > 0 | frontPlotArea > 0))
}

#' Stub method just returns string of TODO
#'  
#' @param length - The number of rows to return
to.do <- function(length){
  rep("TODO", length(length))
}

#' ## External area dimensions
#' 
#' Calculates, if available the dimensions of the front and back, first uses values 
#' indicating existence of front and back plot, if these are caluclated as true width
#' and depth and then derived from widthOfPlot and depthOfBackPlot.
#'
#' @param  widthOfPlot - contains value from which width of plot can be derived
#' 
#' @param doesFrontPlotExist - contains a YES/NO/NA value
#' 
#' @param doesBackPlotExist - contains a YES/NO/NA value
#' 
#' @param depthOfBackPlot - contains value from which width of back plot can be 
#' derived
#' 
#' @param depthOfFrontPlot - contains value from which depth of front plot can be 
#' derived
calc.plot.dimensions <- function(widthOfPlot, doesFrontPlotExist, doesBackPlotExist, 
                                 depthOfBackPlot, depthOfFrontPlot){
  #Frontplot dimensions
  
  #Backplot dimensions
  doesBackPlotExist <- ifelse(is.na(doesBackPlotExist), FALSE, 
                              ifelse(doesBackPlotExist == "Yes", TRUE, FALSE))
  backplotWidth <- ifelse(doesBackPlotExist, 
                          ifelse(is.na(widthOfPlot), 0, widthOfPlot), 0)
  backplotDepth <- ifelse(is.na(depthOfBackPlot),0,depthOfBackPlot)
  
  #Frontplot dimensions
  doesFrontPlotExist <- is.na(doesFrontPlotExist) == FALSE & doesFrontPlotExist == "Yes"
  frontplotWidth <- ifelse(doesFrontPlotExist, 
                           ifelse(is.na(widthOfPlot), 0, widthOfPlot), 0)
  frontplotDepth <- ifelse(is.na(depthOfFrontPlot),0,depthOfFrontPlot)
    
  data.frame(
    backplotWidth <- ifelse(doesBackPlotExist,backplotWidth, 0),
    backplotDepth <- ifelse(doesBackPlotExist,backplotDepth, 0),
    backplotArea <- ifelse(doesBackPlotExist, backplotDepth * backplotWidth, 0),
    
    frontplotWidth <- ifelse(doesFrontPlotExist,frontplotWidth, 0),
    frontplotDepth <- ifelse(doesFrontPlotExist,frontplotDepth, 0),
    frontplotArea <- ifelse(doesFrontPlotExist, frontplotDepth * frontplotWidth, 0))
}

#' ## Occupants
#' 
#' Calculates the number of adults and chidren
#'
#' Returns - data.frame with two columns, adult and children
occupant.counts <- function(aacode, numOfPeopleInHouse, numOfDepdententChildrenInTheHouse){
  calc_numOfPeopleInHouse <- ifelse(is.na(numOfPeopleInHouse),0,numOfPeopleInHouse)
  calc_children <- ifelse(is.na(numOfDepdententChildrenInTheHouse),
                          0,numOfDepdententChildrenInTheHouse)
  
  calc_adults <- calc_numOfPeopleInHouse - calc_children
  calc_adults <-  ifelse(calc_adults < 0, 0, calc_adults)
  calc_adults <-  ifelse(calc_adults < 0, 0, calc_adults)
  
  calc_adults <-  ifelse(calc_adults == 0 & calc_children > 0 , NA, calc_adults)
  
  calc_adults <-  ifelse(calc_adults == 0, "NULL", calc_adults)
  calc_adults <-  ifelse(calc_adults == "", "NULL", calc_adults)
  calc_adults <-  ifelse(is.na(calc_adults), "NULL", calc_adults)
  calc_children <- ifelse(calc_adults == "NULL" & calc_children == 0, "NULL", calc_children)
  
  data.frame(aacode, adults = calc_adults, children = calc_children)
}

#' ## Builtform type
#'
#'Simple look-up table which maps an EHCS survey value for dwelling type to one
#'which can be used by the NHM stock import
#'
#'@param dwellingType - String representing dwelling type from the survey
builtform.type <-function(dwellingType){
  as.factor(checked.revalue(
    dwellingType,c(
      "converted flat" = "ConvertedFlat",
      "semi detached" = "SemiDetached",
      "purpose built flat, low rise" = "PurposeBuiltLowRiseFlat",
      "purpose built flat, high rise" = "PurposeBuiltHighRiseFlat",
      "detached" = "Detached",
      "bungalow" = "Bungalow",
      "end terrace" = "EndTerrace",
      "mid terrace" = "MidTerrace")))}


#' ## Roof ownership

owns.part.of.roof <- function(OWNTYPE){
  return (as.character(levels(roof.look.up(OWNTYPE))[roof.look.up(OWNTYPE)]))
}

#' Determine the ownership of the dwelling in order to determine whether the roof is 
#' owned by the occupants
roof.look.up <- function(OWNTYPE){
  as.factor(checked.revalue(
    OWNTYPE,c(
      "freeholder of house" = TRUE,
      "leaseholder - no share of FH" = FALSE,
      "leaseholder owning FH collectively" = TRUE,
      "leaseholder owning FH of whole bldg" = TRUE,
      "freeholder of flat - owning FH of flat only" = FALSE,
   #   "commonholder (property built as CH)" = TRUE,
    #  "commonholder (property converted to CH)" = TRUE
   "commonholder" = TRUE)))
}
  
#' ## Groundfloor construction type
#' 
#' If either hasSolidKitchenFloor or hasSolidLivingRoomFloor have the value of "NO" 
#' then we assume the floor construction type is solid. This methodology came from 
#' CARS, we could do a more detailed job using floor levels etc... as we have model 
#' for each story in the house.
groundfloor.construction.type <- function(hasSolidKitchenFloor, hasSolidLivingRoomFloor){
      floorType <- ifelse(is.na(hasSolidKitchenFloor), "solid",
                   ifelse(hasSolidKitchenFloor == "Yes", "solid", "suspendedtimber"))  
      floorType <- ifelse(floorType == "solid", 
                          ifelse(
                            is.na(hasSolidLivingRoomFloor), 
                            "solid", 
                            ifelse(hasSolidLivingRoomFloor == "Yes",
                                   "solid","suspendedtimber")),
                          "suspendedtimber")
      
      return (floorType)
}

#' ## Living area fraction
#' 
#' Returns a data frame with the following values:
#' livingArea - this is (livingRoomWidth * livingRoomDepth) 
#' numHabitalRooms - uses supplied value as is
#' numOfBedrooms - always returns zero TODO: don't know why?
#'
#'@param numOfHabitalRooms - FINROOMS (interior)
#'@param livingRoomWidth - FINLIVWI (interior) in meters
#'@param livingRoomDepth - FINLIVDE (interior) in meters
#'@param totalFloorArea - FLOORARE (dimensions) in meters
cal.livingarea.data <- function(numOfHabitalRooms, livingRoomWidth, 
                                livingRoomDepth, totalFloorArea){
  
  
  calc_livingRoomWidth <- ifelse(is.na(livingRoomWidth),0, livingRoomWidth)
  calc_livingRoomWidth <- ifelse(calc_livingRoomWidth == 88.8, 0, calc_livingRoomWidth)
  
  calc_livingRoomDepth <- ifelse(is.na(livingRoomDepth),0, livingRoomDepth)
  calc_livingRoomDepth <- ifelse(calc_livingRoomDepth == 88.8, 0, calc_livingRoomDepth)
  
  calc_livingAreaFaction <- (livingRoomWidth * livingRoomDepth)/ totalFloorArea
  
  calc_livingAreaFaction <- ifelse(totalFloorArea <= 0, 0, 
                          (calc_livingRoomWidth * calc_livingRoomDepth)/ totalFloorArea)
  
  data <- data.frame(
    livingAreaFaction = 0, #calc_livingAreaFaction, # this is not deliberate, but instead a decision taken since
    # there seems to be an error with the living area fraction calaculation and it won't run, sothis is a necessary step.
    # it also aligns the stock better with BRE's pproach for LAF, but this isn't the reason I've done it here
    numHabitalRooms = ifelse(is.na(numOfHabitalRooms), 0, numOfHabitalRooms),
    numOfBedrooms = 0)
  
  return (data)
}


#' ## Access to outdoor space
#' 
#' If front or back plots are not zero in size and plot type is Private or Shared 
#' then returns true,
#' otherwise false.
#'
#'@param typeOfPlot - String
#'@param frontPlotArea - Numeric - Area in m2
#'@param backPlotArea - Numberic - Area in m2
accces.to.outside.space <- function(typeOfPlot, frontPlotArea, backPlotArea){
    #' Check either back or front plot exists
    # backPlotExists = plot.dimensions$backplotDepth > 0 & plot.dimensions$backplotWidth > 0
    # frontPlotExists = plot.dimensions$backplotDepth > 0 & plot.dimensions$backplotWidth > 0
    
    #' If plot is private or shared then return true
}

#' Simple look up table based on floor location type of either ground or basement
#' 
#' @param level - As in the EHCS this is one of bb, BB, gg or GG
floor.level <- function(level){
  as.factor(checked.revalue(
    level,c(
      "gg" = "GROUND",
      "GG" = "GROUND",
      "bb" = "BASEMENT",
      "BB" = "BASEMENT"
      )))
}
