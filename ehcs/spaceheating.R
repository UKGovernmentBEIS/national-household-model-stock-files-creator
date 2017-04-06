#' ---
#' title: "Spaceheating functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make spaceheating data
#'
#'Create a .csv file using the function make.spaceheating, which creates a dataframe 
#'containing a complete set of populated variables for the space-heating.csv stock 
#'file.
#'
#'The space-heating.csv file contains a set of information on the heating systems, 
#'including fuel type, efficencies and heating control systems. Storage heaters and 
#'storage combi require additional information if they are present in a dwelling, 
#'otherwise these columns do not need to be populated
#'
#' @param allEntries - combined sav files data.frame from the EHS data
space.heating.make <- function(allEntries, path.to.ehcs, path.to.output){
  use.sedbuk <- if (exists("option.ehs.spaceheating.sedbuk")) option.ehs.spaceheating.sedbuk
                else TRUE
  if (use.sedbuk) {
      run.sedbuk <- if (exists("option.ehs.spaceheating.sedbuk.run")) option.ehs.spaceheating.sedbuk.run
                    else FALSE

      # Run sedbuk matching java code if needed.
      if (run.sedbuk) {
          sedbukLookUp <- sedbuk.look.up.table(allEntries)
          write.csv(sedbukLookUp, file=file.path(path.to.ehcs, "r_sedbuk.csv"), row.names=FALSE)

          jarFileLocation <- paste(file.path(getwd(), "ehcs", "boilermatcher-all-1.0.0.jar"))
          fileLocations <- paste(file.path(path.to.ehcs, "r_sedbuk.csv"),
                                 file.path(path.to.output, "sedbuk_matches.csv"))
          javaCommand <- paste("java -jar", jarFileLocation, fileLocations)
          print(paste("Executing java sedbuk look-up with command: ",javaCommand))
          system(command = javaCommand)
      }

      ## Read in the created sebuk match results
      sedbuk <- read.csv(file.path(path.to.output, "sedbuk_matches.csv"))
  }
  
  ## match remaining cases against look up tables - we loop over the lookup tables
  ## specified in option.ehs.spaceheating.lookups.

  unmatched.cases <- if (use.sedbuk) subset(allEntries, !(aacode %in% sedbuk$aacode))
                     else allEntries

  ## supply a default set of lookups if not defined elsewhere (e.g. by main function)
  lookups <- if (exists("option.ehs.spaceheating.lookups"))
                 option.ehs.spaceheating.lookups
             else
                 c("lup-boiler-4b.csv",
                   "electric-boiler-lup.csv",
                   "gas-room-heater-lup.csv",
                   "oil-room-heater-lup.csv",
                   "other-room-heater-lup.csv",
                   "solid-fuel-boiler-lups.csv",
                   "storage-heater-lup.csv",
                   "warm-air-lup.csv",
                   "Finchbcd-lup.csv",
                   "space-heating-missing-data-lup.csv")


  for (lup in lookups) {
      new.matched.cases <- heating.matching(
          path.to.ehcs,
          unmatched.cases,
          lup)
      unmatched.cases <- subset(unmatched.cases, !(aacode %in% new.matched.cases$aacode))
      matched.cases <- if (exists("matched.cases")) rbind(matched.cases, new.matched.cases)
                       else new.matched.cases
  }

  #combine all spaceheating with the main table
                                        #gather information for the sedbuk matched data into a data frame
  if (use.sedbuk) {
      matched.sedbuk <- data.frame(
          aacode=sedbuk$aacode
         ,basicefficiency=sedbuk$annualefficiency
         ,winterefficiency=sedbuk$winterefficiency
         ,summerefficiency=sedbuk$summerefficiency
         ,spaceheatingsystemtype=sedbuk.boiler.to.spaceheating.type(sedbuk$boilertype)
         ,fluetype=sedbuk$fluetype
         ,mainheatingfuel=sedbuk$fueltype
         ,iscondensing=sedbuk$condensing
                                        #,matchSource=rep("sedbuk", length(sedbuk$aacode))
         ,isstoragecombicylinderthemostatpresent=rep("NULL", nrow(sedbuk))
         ,isstoragecombicylinderfactoryinsulated=rep("NULL", nrow(sedbuk))
         ,storageheatertype=rep("", nrow(sedbuk))
         ,PcdbMatch=rep(T,nrow(sedbuk))
      )
  }
  #gather information for the SAP table 4a/4b and Fincbcd matched data into a second
  #data frame of a similar structure
  matched.cases <-data.frame(
      aacode=matched.cases$aacode
     ,basicefficiency=matched.cases$basicefficiency
     ,winterefficiency=matched.cases$winterefficiency
     ,summerefficiency=matched.cases$summerefficiency
     ,spaceheatingsystemtype=tolower(matched.cases$spaceheatingsystemtype)
     ,fluetype=tolower(matched.cases$fluetype)
     ,mainheatingfuel=tolower(matched.cases$mainheatingfuel)
     ,iscondensing=as.factor(matched.cases$iscondensing)
     #,matchSource=rep("lookup", length(matched.cases$aacode))
     ,isstoragecombicylinderthemostatpresent=rep("NULL", nrow(matched.cases))
     ,isstoragecombicylinderfactoryinsulated=rep("NULL", nrow(matched.cases))
     ,storageheatertype=tolower(matched.cases$storageheatertype)
     ,PcdbMatch=rep(F,nrow(matched.cases))
  )

  if (use.sedbuk) {
      matched.heating <- rbind(matched.sedbuk,matched.cases)
      ##collect additional info on storage combi boilers from the sedbuk data (storage
      ##combis can only be identified through sedbuk information)
      sedbuk<-subset(sedbuk,select=c("aacode","storeboilervolume"
                                    ,"storesolarvolume","storeinsulationthickness"))
      ##change storage combi boiler variables to match DTO nomenclature
      setnames(sedbuk, c("aacode", "storagecombicylindervolume",
                         "storagecombisolarvolume","storagecombicylinderinsulationthickness"))
      ## join additional storage combi boilers to the combined heating system match data
      matched.heating <- join(matched.heating,sedbuk,by="aacode")
  } else {
      matched.heating <- matched.cases
      matched.heating$storagecombicylindervolume <- NA
      matched.heating$storagecombisolarvolume <- NA
      matched.heating$storagecombicylinderinsulationthickness <- NA
  }

  ##additional information for other heating system types, plus boiler installation
  ##year and secondary heating information (using additional functions defined below)
  allHeatingSysteams <- data.table(
      aacode = allEntries$aacode
     ,electrictariff = electricity.tariff(allEntries$Finmhfue)
     ,heatingsystemcontroltypes = heating.control.types(allEntries$Finchtim,
                                                        allEntries$Finchrom,
                                                        allEntries$Finchtrv,
                                                        allEntries$Finchtzc,
                                                        allEntries$Finchdst)
     ,installationyear = installation.year(rep("NOT", length(allEntries$aacode)),
                                           allEntries$Finwhxag,
                                           allEntries$Finchbag,
                                           allEntries$fodconac)
     ,secondaryheatingsystemtype = secondary.heating(allEntries$Finoheat,
                                                     allEntries$Finohtyp)
   )
  
  #add additional data to main matched.heating table 
  matched.heating <- join(matched.heating,allHeatingSysteams,by="aacode")

  #Add details on community heating
  allHeatingSysteams <- data.table(
    aacode = matched.heating$aacode
    ,chpfraction = ifelse(matched.heating$spaceheatingsystemtype == 
                            "community_heating_with_chp",as.character(0.35), "")
    ,communitychargingusagebased = 
      ifelse(matched.heating$spaceheatingsystemtype == "community_heating_with_chp" |
        matched.heating$spaceheatingsystemtype == "community_heating_without_chp", TRUE,
        FALSE)
  )
  matched.heating <- join(matched.heating,allHeatingSysteams,by="aacode")
  
  #Add storage heater control types
  storageHeaterSurveyVars <- subset(allEntries,select = c(aacode, Finchctc, Finchacc))
  allHeatingSysteams <- join(matched.heating, storageHeaterSurveyVars, by = "aacode")
  matched.heating <- join(matched.heating,
                          storage.heater.control.type(allHeatingSysteams),by="aacode")

  #convert heating efficiencies to proportions from percentages
  matched.heating$basicefficiency <- matched.heating$basicefficiency / 100
  matched.heating$winterefficiency <- matched.heating$winterefficiency / 100
  matched.heating$summerefficiency <- matched.heating$summerefficiency / 100
  
  #print success message
  print(paste("spaceheating DTO complete; number of records: ", nrow(matched.heating)))
  ## Return the compiled data
  return (matched.heating)
}

#' ## Sedbuk boilers: rename
#' 
#' Converts the boiler type returned from the sedbuk match to a
#' space heating systemt typr
#'
#'
#' @param boilerType - one of CPSU | INSTANT_COMBI | STORAGE_COMBI | REGULAR
sedbuk.boiler.to.spaceheating.type <- function(boilerType){
  as.factor(checked.revalue(
    boilerType,c(
      "CPSU" = "cpsu"
      ,INSTANT_COMBI = "combi"
      ,STORAGE_COMBI = "storage_combi"
      ,REGULAR =  "standard")))}

#' ## Heating system installation year
#' 
#' @param spaceHeatingType
#' @param backBoilerAge - Finwhxag
#' @param boilerAge - Finchbag
#' @param buildYear - fodconac
installation.year <- function(spaceHeatingType, backBoilerAge, boilerAge, buildYear){
  isBackBoiler <- spaceHeatingType == "BACK_BOILER" | spaceHeatingType == 
                                                    "BACK_BOILER_NO_CENTRAL_HEATING"
  
  calcBoilerAge <- ifelse(isBackBoiler, age.or.buildYear(backBoilerAge, buildYear), 
                                                                              "NULL")
  calcBoilerAge <- ifelse(calcBoilerAge == "NULL", 
                          age.or.buildYear(boilerAge, buildYear), calcBoilerAge)
  
  return (calcBoilerAge)
}

#' Returns either the survey year 2014 - age of boiler or if age of boiler is 88 or 
#' na then build year of the property
#'
#'@param age - of boiler
#'@param buildYear - of house case
age.or.buildYear <- function(age, buildYear){
  boilerAge <- ifelse(is.na(age) | age == 88, buildYear, 2014 - age)
  return(boilerAge)
}

#' ## Electricity tariff
#' 
#' @param Finmhfue - main heating system fuel
electricity.tariff <- function(Finmhfue){
  as.factor(checked.revalue(
    Finmhfue,c(
      "Gas - Mains" = "NULL",
      "Gas - Bulk/LPG" = "NULL",
      "Gas - bottled" = "NULL",
      "Oil" = "NULL",
      "Solid fuel - coal" = "NULL",
      "Solid fuel - smokeless fuel" = "NULL",
      "Solid fuel - anthracite" = "NULL",
      #"Solid fuel - wood" = "NULL",
      "Biomass" = "NULL", #CMT - fuel variable name changed
      "Electricity - standard" = "FLAT_RATE",
      "Electricity - 7hr tariff" = "ECONOMY_7",
      "Electricity - 10hr tariff" = "ECONOMY_10",
      "Electricity - 24hr tariff" = "TWENTYFOUR_HOUR_HEATING",
      "Communal - CHP/Waste heat" = "NULL",
      "Communal - from boiler" = "NULL"
    )))}


#' ##Heating system controls
#' 
#' Returns a delimeted list (";") of control types present in house case
#'
#' @param Finchtim - Progammer present
#' @param Finchrom - Room Thermostat present
#' @param Finchtrv - TRV present
#' @param Finchtzc - Time temperature zone control present
#' @param Finchdst - Delaye start thermostat
heating.control.types <- function(Finchtim, Finchrom, Finchtrv, Finchtzc, Finchdst){
  is.yes <- function(X, label) ifelse(is.na(X),"", ifelse(X == "Yes", label, ""))
  remove.blanks <- function(X) 
          str_replace_all(
            str_replace_all(
              str_replace_all(
                str_replace_all(X, ";;;" ,";"), ";;", ";"), "^;", ""), ";$", "")
  controls <-
    remove.blanks(
    paste(
      sep=";",
      is.yes(Finchtim, "programmer"),
      is.yes(Finchrom, "room_thermostat"),
      is.yes(Finchtrv, "thermostaticradiatorvalve"),
      is.yes(Finchtzc, "timetemperaturezonecontrol"),
      is.yes(Finchdst, "delayed_start_thermostat")
      )
    )
  
  
  return (controls)  
}

#' ##Storage heater controls
#' 
#' With only heating systems that are storage heaters return a data frame where each 
#' row has a value for storageheatercontroltype
#' 
#' @param allHeatingSysteams - data.frame of columns:
#'                                       spaceheatingsystemtype | Finchacc | Finchctc
storage.heater.control.type <- function(allHeatingSysteams){
  onlyStorageHeaters <- subset(allHeatingSysteams, 
                               spaceheatingsystemtype == "storage_heater")
  celectChargeControl <- subset(onlyStorageHeaters, Finchctc == "Yes")
  
  others <- subset(onlyStorageHeaters, 
                   !onlyStorageHeaters$aacode %in% celectChargeControl$aacode)
  automaticChargeControl <- subset(others, Finchacc == "Yes")
  manualChargeControl <- subset(others, 
                                !others$aacode %in% automaticChargeControl$aacode)
  
  celectChargeControl <- data.table(
    aacode = celectChargeControl$aacode,
    storageheatercontroltype = rep("CELECT_CHARGE_CONTROL", 
                                   length(celectChargeControl$aacode)))
  
  automaticChargeControl <- data.table(
    aacode = automaticChargeControl$aacode,
    storageheatercontroltype = rep("AUTOMATIC_CHARGE_CONTROL", 
                                   length(automaticChargeControl$aacode)))
  
  manualChargeControl <- data.table(
    aacode = manualChargeControl$aacode,
    storageheatercontroltype = rep("MANUAL_CHARGE_CONTROL", 
                                   length(manualChargeControl$aacode)))
  
  storageHeaterControls <- rbind(celectChargeControl, automaticChargeControl)
  storageHeaterControls <- rbind(storageHeaterControls, manualChargeControl)
  
  return (storageHeaterControls)
}

#' ##Storage combination boiler information
#' 
#' If the boiler type is STORAGE_COMBI then Sets cylinder and boiler store volumes 
#' including solar store and insulatin thickness of combi cylinder. 
#' Otherwise set's values to null.
#'
#' @param 
storage.combi <- function(sedbuk.match){
  isStorage <- matches$boilertype == "STORAGE_COMBI" 
  
  details <- data.table(
    aacode =  matches$aacode
    ,storagecombicylindervolume = ifelse(isStorage, matches$storeboilervolume, "NULL") 
    ,storagecombisolarvolume = ifelse(isStorage, matches$storesolarvolume, "NULL")
    ,storagecombicylinderinsulationthickness = ifelse(isStorage, 
                                                      matches$storeinsulationthickness)
    ,isstoragecombicylinderthemostatpresent = "NULL"
    ,isstoragecombicylinderfactoryinsulated = "NULL"
  )
  
  return (details)
}

#' ##Secondary heating systems
#' 
#' Returns secondary heating system type
#'
#'@param FINOHEAT - Is secondary heating type present
#'@param FINOHTYP - Other Heating Type of System
secondary.heating <- function(FINOHEAT, FINOHTYP){
  isPresent = ifelse(FINOHEAT == "Yes", TRUE, FALSE)
  
  return (secondaryheatingsystemtype = ifelse(isPresent, 
                                              secondary.heating.type(FINOHTYP),
                                              "NO_SECONDARY_SYSTEM"))
}

#' Returns a character string describing the type of secondary heating
#' 
#' @note Does not accept NA values
#' @paran FINOHTYP - Other Heating Type of System
secondary.heating.type <- function(FINOHTYP){
  type <- (as.character(levels(
    secondary.heating.type.lookup(FINOHTYP))[secondary.heating.type.lookup(FINOHTYP)]))
  return(type)
}

#' Simple conversion from Survey value to NHM value for secondary heating type
#' @param FINOHTYP -  Other Heating Type of System
secondary.heating.type.lookup <- function(FINOHTYP){
  as.factor(checked.revalue(
    FINOHTYP,c(
      "Mains gas - open flue" = "GAS_FIRE_OPEN_FLUE"
      ,"Mains gas - balanced flue" = "GAS_FIRE"
      ,"Mains gas - fan assisted"  = "GAS_FIRE"
      ,"Mains gas - condensing" =  "GAS_FIRE"                
      ,"Mains gas - live effect - sealed to chimney" = "GAS_COAL_EFFECT_FIRE"
      ,"Mains gas - live effect - fan assisted flue" = "GAS_COAL_EFFECT_FIRE"
      ,"Mains gas - decorative - open to chimney" = "GAS_COAL_EFFECT_FIRE"
      ,"Mains gas - flueless" = "GAS_FIRE_FLUELESS"          
      ,"Mains gas - unknown" = "GAS_FIRE_FLUELESS"
      ,"LPG - fixed heaters" = "GAS_FIRE"
      ,"Electric heaters - panel/convector or radiant" = "ELECTRIC_ROOM_HEATERS"
      ,"Electric heaters - portable" = "ELECTRIC_ROOM_HEATERS"
      ,"Electric heaters - individual storage heater" = "ELECTRIC_ROOM_HEATERS"
      ,"Solid fuel heaters - open fire" = "OPEN_FIRE"
      ,"Solid fuel heaters - stove/space heater" = "OPEN_FIRE"
      ,"Paraffin - portable heaters" = "GAS_FIRE" 
      ,"Other" = "GAS_FIRE"
    )))
}

#' ##Sedbuk boiler matching
#' 
#' ###Pre-processing of variables
#' 
#' Creates a data.table that has the required inputs needed to search the sedbuk 
#' index for a boiler match
#' 
#'@return data.table with columns aacode, make, model, fuel, flue, type
sedbuk.look.up.table <- function(allEntries){
  look.up.table <- data.table(
    aacode = allEntries$aacode
    ,make =  as.character(allEntries$Finchbma)
    ,model = as.character(allEntries$Finchbmo)
    ,fuel =  sedbuk.fuelType(allEntries$Finmhfue)
    ,flue = rep("", length(allEntries$aacode))
    ,type = sedbuk.boiler.type(allEntries$Finmhboi)
  )
  
  return (look.up.table)
}

#' ###Rename of boiler systems returned from sedbuk matching
#'
#'type - REGULAR | INSTANT_COMBI | STORAGE_COMBI | CPSU | UNKNOWN |
#'
sedbuk.boiler.type <- function(Finmhboi){
  as.factor(checked.revalue(
    Finmhboi,c(
      "Standard" = "REGULAR",
      "Back boiler" = "UNKNOWN",
      "Combination" = "INSTANT_COMBI",
      "Condensing" = "REGULAR",
      "Condensing combi" = "INSTANT_COMBI",
      "Combined primary storage unit" = "CPSU",
      "No boiler" = "EMPTY"
    )))
}

#' ##Main heating system fuel type
#' 
#' Converts from EHS fuel type to a variable the sedbuk boiler match recoginises,
#' all other fuel types are set to NULL
#'
#' @param Finmhfue - main heating system fuel
sedbuk.fuelType <- function(Finmhfue){
  as.factor(checked.revalue(
    Finmhfue,c(
      "Gas - Mains" = "MAINS_GAS",
      "Gas - Bulk/LPG" = "BULK_LPG",
      "Gas - bottled" = "BULK_LPG",
      "Oil" = "OIL",
      "Solid fuel - coal" = "NULL",
      "Solid fuel - smokeless fuel" = "NULL",
      "Solid fuel - anthracite" = "NULL",
      #"Solid fuel - wood" = "NULL",
      "Biomass" = "NULL",   #CMT - fuel variable name changed
      "Electricity - standard" = "NULL",
      "Electricity - 7hr tariff" = "NULL",
      "Electricity - 10hr tariff" = "NULL",
      "Electricity - 24hr tariff" = "NULL",
      "Communal - CHP/Waste heat" = "NULL",
      "Communal - from boiler" = "NULL"
    )))
}

nhm.fuelType <- function(Finmhfue){
  as.factor(checked.revalue(
    Finmhfue,c(
      "Gas - Mains" = "MAINS_GAS",
      "Gas - Bulk/LPG" = "BULK_LPG",
      "Gas - bottled" = "BOTTLED_LPG",
      "Oil" = "OIL",
      "Solid fuel - coal" = "HOUSE_COAL",
      "Solid fuel - smokeless fuel" = "HOUSE_COAL",
      "Solid fuel - anthracite" = "HOUSE_COAL",
      #"Solid fuel - wood" = "BIOMASS_WOOD",
      "Biomass" = "BIOMASS_WOOD",    #CMT - fuel variable name changed
      "Electricity - standard" = "ELECTRICITY",
      "Electricity - 7hr tariff" = "ELECTRICITY",
      "Electricity - 10hr tariff" = "ELECTRICITY",
      "Electricity - 24hr tariff" = "ELECTRICITY",
      "Communal - CHP/Waste heat" = "MAINS_GAS",
      "Communal - from boiler" = "MAINS_GAS"
    )))
}

#'##Boiler matching: SAP lookups
#'
#'This function imports data from heating look up tables
#' 
#'These file must be available in the same folder as the scottish stock
#'  
#'@param path.to.ehcs - the path to the ehcs data (used to find boiler lookup tables)
#' 
#'@param unmatched - the wide SPSS data frame containing only those cases which we have left to match
#' 
#'@param name.to.heatinglookup - the filename of the lookup table to use. this file should have the matching columns on the left
#'
heating.matching <- function(path.to.ehcs,
                             unmatched,
                             name.to.heatinglookup) {
    boiler <- read.csv(file.path(path.to.ehcs,"boilerLookUps", name.to.heatinglookup),
                       header=TRUE)

    ## we match on the intersection of columns in the lookup table
    ## and columns in the raw data.
    match.columns <- colnames(boiler)
    match.columns <- match.columns[match.columns %in% colnames(unmatched)]

    ## boiler table may be missing some specifics, in which case we need some NAs
    if (("installationyear" %in% colnames(boiler))==FALSE){
        boiler$installationyear <- NA
    }
    if (("winterefficiency" %in% colnames(boiler))==FALSE){
        boiler$winterefficiency <- NA
    }
    if (("summerefficiency" %in% colnames(boiler))==FALSE){
        boiler$summerefficiency <- NA
    }

    ## reduce the unmatched data down to only the columns we need (makes the join faster)
    unmatched <- unmatched[, c("aacode", match.columns)]

    ## do the join - we want an inner join (i.e. only rows that are in both sets)
    matched <- join(unmatched,boiler, by=match.columns, type="inner")

    ## finally the results - we want only to keep certain columns
    matched[, c("aacode", "winterefficiency", "summerefficiency"
               ,"basicefficiency", "spaceheatingsystemtype", "fluetype"
               ,"mainheatingfuel", "iscondensing", "installationyear"
               ,"storageheatertype")]
}
