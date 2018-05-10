#' ---
#' title: "Waterheating functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make waterheating data
#'
#'Create a .csv file using the function make.waterheating, which creates a dataframe 
#'containing a complete set of populated variables for the water-heating.csv stock 
#'file.
#'
#'The water-heating.csv file contains a set of information on the hot water heating 
#'systems, including fuel type and efficencies. However, a significant majority of 
#'dwellings will have their hot water provided by their space heating systems (e.g. 
#'mains gas combi boilers or standard boilers with a water tank). In these instances,
#'much less information is required in the water-heating.csv and data that describes 
#'the efficiency or fuel used by the water heating system will be taken from the 
#'space heating system for that case.
#'
#'Otherwise the water heating data in 'Table 4a: Heating systems (space and water)' 
#'in the SAP 2012 documentation is used to allocate heating system types and 
#'efficiencies where a dwelling has separate space heating and water heating systems.
#'
#' @param allEntries - combined sav files data.frame from the EHS data
#' 
#' @param - spaceHeatingDTO - the space heating dto file created in a previous script
#' 
#' @param - path.to.ehcs - the file path to the ehcs survey data
make.waterheating <- function(allEntries, spaceHeatingDTO, path.to.ehcs){
  # Make entries for those that should be with central heating
  waterHeatingWithCentralHeating <- data.table(subset(allEntries
                                              , watersys == "with central heating"))
  allWaterHeating <- data.table(
    aacode = waterHeatingWithCentralHeating$aacode
    ,withcentralheating = rep(TRUE, length(waterHeatingWithCentralHeating$aacode))
    ,basicefficiency = rep(0, nrow(waterHeatingWithCentralHeating))
  )
  
  # Create efficiencies and system type information for those that don't use Central 
  # Heating
  withCentral.id <-data.frame(aacode=allWaterHeating$aacode)
  matched.cases <- water.system.matching(allEntries,path.to.ehcs,withCentral.id,
                        "water-heating-singlepoint-lup.csv",c("watersys","Finwspty"))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                    ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                    "water-heating-single-immrsn-lup.csv",c("watersys","Finwsity")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                     ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                     "water-heating-community-lup.csv",c("watersys","Finwhlty")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                     ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                     "water-heating-other-boiler-lup.csv",c("watersys","Finwhoty")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                     ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                     "water-heating-multipoint-lup.csv",c("watersys","Finwmpty")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                     ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                     "water-heating-double-immrsn-lup.csv",c("watersys","Finwdity")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                     ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                     "water-heating-comm-boiler-lup.csv",c("watersys","Finwhlty")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                     ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                     "water-heating-back-boiler-lup.csv",c("watersys","Finwhxty")))
  matched.cases <- rbind(matched.cases,water.system.matching(allEntries,path.to.ehcs
                      ,rbind(withCentral.id,data.frame(aacode=matched.cases$aacode)),
                      "water-heating-missing-data-lup.csv",c("watersys")))
  # Set flag for with central heating to false for all these cases
  matched.cases$withcentralheating <- rep(FALSE, nrow(matched.cases))
  allWaterHeating <- rbind(allWaterHeating, matched.cases, fill = TRUE)
  
  # Add Cylinders
  cylinderProperties <- cylinder.properties(allEntries)
  allWaterHeating <- join(allWaterHeating, cylinderProperties, by = "aacode")

  # Add Immersion Heaters
  immersionHeaters <- immersion.heater(allEntries)
  allWaterHeating <- join(allWaterHeating, immersionHeaters, by = "aacode")
  
  # Solar Hot Water
  hasSolarHotWater <- subset(allEntries, Finwotfu == 15)
  
  noSolarHotWater <- subset(allWaterHeating
                            , !allWaterHeating$aacode %in% hasSolarHotWater$aacode)
  noSolarHotWater <- data.table(
    aacode = noSolarHotWater$aacode
    ,solarhotwaterpresent = rep(FALSE, nrow(noSolarHotWater))
    ,solarstoreincylinder = rep(FALSE, nrow(noSolarHotWater))
    ,solarstorevolume =  rep(as.factor(0), nrow(noSolarHotWater))
  )
  
  # Uses main hot water cylinder
  solarstoreincylinder <- subset(allWaterHeating,
  allWaterHeating$aacode %in% hasSolarHotWater$aacode & !allWaterHeating$volume == 0)
  solarstoreincylinder <- data.table(
    aacode = solarstoreincylinder$aacode
    ,solarhotwaterpresent = rep(TRUE, nrow(solarstoreincylinder))
    ,solarstoreincylinder = rep(TRUE, length(solarstoreincylinder$aacode))
    ,solarstorevolume =  solarstoreincylinder$cylindervolume
  )
  
  # Uses additional hot water cylinder - TODO Slight descrepency between num of has 
  # SolarHotWater and sum of solarstoreincylinder and solarStoreInAdditionalCylinder
  solarStoreInAdditionalCylinder <- subset(allWaterHeating,
                         allWaterHeating$aacode %in% hasSolarHotWater$aacode 
                         & !allWaterHeating$aacode %in% solarstoreincylinder$aacode)
  
  solarStoreInAdditionalCylinder <- data.table(
    aacode = solarStoreInAdditionalCylinder$aacode
    ,solarhotwaterpresent = rep(TRUE, nrow(solarStoreInAdditionalCylinder))
    ,solarstoreincylinder = rep(FALSE, length(solarStoreInAdditionalCylinder$aacode))
    ,solarstorevolume =  rep(as.factor(75), 
                             length(solarStoreInAdditionalCylinder$aacode))
  )
  
  solarStoreCylinders <- rbind(noSolarHotWater, solarstoreincylinder, 
                               solarStoreInAdditionalCylinder)
  allWaterHeating <- join(allWaterHeating, solarStoreCylinders, by = "aacode")
  
  #Add final columsn for unknown values
  allWaterHeating$installationyear <- rep("", nrow(allWaterHeating))
  allWaterHeating$chpfraction <- rep("", nrow(allWaterHeating))
  allWaterHeating$communitychargingusagebased <- rep("", nrow(allWaterHeating))
  allWaterHeating$electrictariff <- rep("", nrow(allWaterHeating))
      
  #Return the compiled data
  allWaterHeating$cylindervolume <- revalue(allWaterHeating$cylindervolume, 
                                            c("0" = ""))
  
  allWaterHeating$basicefficiency <- allWaterHeating$basicefficiency / 100
  allWaterHeating$winterefficiency <- allWaterHeating$winterefficiency / 100
  allWaterHeating$summerefficiency <- allWaterHeating$summerefficiency / 100

  allWaterHeating$PcdbMatch <- rep(F,nrow(allWaterHeating))
  allWaterHeating <- join(allWaterHeating, HAS_ELECTRIC_SHOWER(allEntries), by = "aacode")
    
  print(paste("water heating DTO complete; number of records: ", nrow(allWaterHeating)))
  return (allWaterHeating)
}

#' Returns a data frame indicating whether the property has an electric shower or not
#' @param allEntries - combined sav files from the EHS
HAS_ELECTRIC_SHOWER <- function(allEntries){
    water.heating.details <- data.frame(
        aacode = allEntries$aacode
       ,region = allEntries$gorEHS
       ,dwellingcaseweight =  allEntries$aagpd1314
       ,has.multi.point = allEntries$Finwmppr == "Yes"
       ,has.single.point = allEntries$Finwsppr == "Yes"
       ,multi.point.is.electric = allEntries$Finwmpty == "Standard"
       ,single.point.is.electric = allEntries$Finwspty == "Standard"
    )
    
    has.electric <- transmute(.data=water.heating.details,
                              aacode
                             #,region
                             #,dwellingcaseweight
                             ,is.single.electric = has.multi.point &  multi.point.is.electric
                             ,is.multi.electric = has.single.point & single.point.is.electric)

    has.electric <- transmute(.data=has.electric
                             ,aacode
                             ,hasElectricShower = is.single.electric | is.multi.electric)
    
    return(has.electric)
}    

#' ## Immersion heater system type
#' 
immersion.heater <- function(allEntries){
  DUAL_COIL <- subset(allEntries, allEntries$Finwdipr == "Yes")
  DUAL_COIL <- data.table(
     aacode = DUAL_COIL$aacode,
     immersionheatertype = "DUAL_COIL"
    )

  # There are some entries where both DUAL_COIL and SINGLE_COIL is yes so exclude 
  # those from SINGLE_COIL
  SINGLE_COIL <- subset(allEntries, 
                        allEntries$Finwsipr == "Yes" & allEntries$Finwdipr == "No")
  SINGLE_COIL <- data.table(
    aacode = SINGLE_COIL$aacode,
    immersionheatertype = "SINGLE_COIL"
  )
  
  heaters <- rbind(SINGLE_COIL, DUAL_COIL)
  
  return(heaters)
}

#' ## Hot water cylinders 
#' 
#' ### Hot water cylinder volume
#' 
#' If Cylinder is present and has volume then return converted volume
#' If cylinder is present but volume is unknown the set as default 110
#' otherwise set volume to 0
#'
#' @param allEntries - combined sav files data.frame from the EHS data
cylinder.properties <- function(allEntries){
  knownCylinderVolumes <- subset(allEntries, is.na(Finwhsiz) == FALSE, c(aacode, 
                                                                         Finwhsiz))
  
  knownCylinderVolumes <- data.table(
    aacode = knownCylinderVolumes$aacode
    ,cylindervolume = cylinder.volume(knownCylinderVolumes$Finwhsiz)
    )
  
  unKnownCylinderVolumes <- subset(allEntries, 
                                !allEntries$aacode %in% knownCylinderVolumes$aacode 
                                  & allEntries$Finwhcyl == "Yes")
  unKnownCylinderVolumes <- data.table(
    aacode = unKnownCylinderVolumes$aacode
    ,cylindervolume = as.factor(110)
  )
  
  cylinderProperties <- rbind(knownCylinderVolumes, unKnownCylinderVolumes)
  
  noCylinderInformation <- subset(allEntries, 
                                  !allEntries$aacode %in% cylinderProperties$aacode)
  noCylinderInformation <- data.table(
    aacode = noCylinderInformation$aacode
    ,cylindervolume = as.factor(0)
  )
  
  cylinderProperties<- rbind(cylinderProperties, noCylinderInformation)
  cylinderProperties$cylindervolume < revalue(cylinderProperties$cylindervolume, 
                                                                        c("0" = "")) 
  
  #InsulatiOn cylinder thickness and type
  isFactoryInsulated <- subset(allEntries, allEntries$Finwhins == "Foam")
  hasThermostat <- subset(allEntries, allEntries$Finwhthe == "Yes")
  
  cylinderThickness <- data.table(
    aacode = allEntries$aacode
    ,cylinderinsulationthickness = cylinder.thickness(allEntries$Finwhmms)
    ,cylinderfactoryinsulated = allEntries$aacode %in% isFactoryInsulated$aacode
    ,cylinderthermostatpresent = allEntries$aacode %in% hasThermostat$aacode
  )
  
  cylinderProperties <- join(cylinderProperties, cylinderThickness, by = "aacode")  
    
  return(cylinderProperties)
}

#' ### Hot water cylinder insulation thickness
#' 
#' Converts survey string value for cylinder insulation thickness into a numeric 
#' factor
#' 
#' @param Finwhmms - cylinder insulation thickness
cylinder.thickness <- function(Finwhmms){
  as.factor(checked.revalue(
    Finwhmms,c(
      "0" = 0,
      "12.5mm" = 12.5,
      "25 mm" = 25,
      "38 mm" = 38,
      "50 mm" = 50,
      "80 mm" = 80,
      "100 mm" = 100,
      "150 mm" = 150
    )))}

#' ### Hot water cylinder volume 

cylinder.volume <- function(Finwhsiz){
  as.factor(checked.revalue(
    Finwhsiz,c(
      "450 x 900mm (110 l)" = 110,
      "450 x 1050mm (140 l)" = 140,
      "450 x 1500mm (210 l)" = 210,
      "450 X 1650mm (245 l)" = 245
    )))}

#' ## Matching independant hot water systems 
#' 
#' This function uses a lookup table to determine the hot water heating system where 
#' the survey specifies that it is not from mains heating (variable = watersys)
#' 
#' hot water heating system is obtain from a lookup table created using EHS variables
#' and data contained in SAP table 4a tables on heating and hot water systems

water.system.matching <- function(allEntries,path.to.ehcs,
                                  matched.id,
                                  name.to.heatinglookup,
                                  columns.to.match) {
  boiler <- read.csv(file.path(path.to.ehcs,"boilerLookUps", name.to.heatinglookup),
                        header=TRUE)
  
  unmatched <- subset(allEntries,!allEntries$aacode %in% matched.id$aacode
                      ,select=c("aacode",columns.to.match))
  
  unmatched<-join(unmatched,boiler,by=columns.to.match)
  
  matched<-subset(unmatched,is.na(unmatched$Heating.System)==FALSE
                  ,select=c(aacode, winterefficiency, summerefficiency
                            ,basicefficiency, waterheatingsystemtype, fluetype
                            ,mainheatingfuel))
  
  return(matched)
}
