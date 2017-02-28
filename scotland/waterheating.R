#' ---
#' title: "Waterheating functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save waterheating data
#'
#'Create a .csv file using the function make.waterheating, which creates a dataframe 
#'containing a complete set of populated variables for the water-heating.csv stock 
#'file.
#'
#'The water-heating.csv file contains a set of information on the hot water heating 
#'systems, including fuel type and efficencies. However, a significant majority of 
#'dwellings will have their hot water provide by their space heating systems (e.g. 
#'mains gas combi boilers or standard boilers with a water tank). In these instances,
#'much less information is required in the water-heating.csv and data that describes 
#'the efficiency or fuel used by the water heating system will be taken from the 
#'space heating system for that case.
#'
#'Otherwise the water heating data in 'Table 4a: Heating systems (space and water)' 
#'in the SAP 2012 documentation is used to allocate heating system types and 
#'efficiencies where a dwelling has separate space heating and water heating systems.
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
#' 
#'@param path.to.input - the path to the folder where the survey data and lookup
#' tables are placed
#'
#'@param path.to.outputs - the path to the output folder
save.waterheating <- function(shcs, output, path.to.input, path.to.output) {
  write.csv(make.waterheating(shcs,path.to.input,path.to.output)
            , file=output, row.names=FALSE, na ="NULL")
}

#'Make the dataframe that contains all the information required for the
#' waterheating.csv file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
#'
#'@param path.to.input - the path to the folder where the survey data and lookup
#' tables are placed
#'
#'@param path.to.outputs - the path to the output folder
make.waterheating <- function(shcs,path.to.input,path.to.output) {
  #First the waterheating dataframe is created from information contained in the
  #scottish survey, the spaceheating.csv and the waterheating look up table
  waterheating <- create.waterheating(shcs,path.to.output,path.to.input)
  
  the.basicefficiency <- wh.basic.efficiency(waterheating$basicefficiency
                                          ,waterheating$spaceheatingbasicefficiency
                                          ,waterheating$withcentralheating)
  the.chpfraction <- wh.chp.fraction(waterheating$M18
                                     ,waterheating$withcentralheating)
  the.communitychargingusagebased <- 
    wh.community.chargingusagebased(waterheating$M18,waterheating$withcentralheating)
  the.electrictariff <- electric.tariff(waterheating$L2,waterheating$M2)
  the.fluetype <- wh.flue.type(waterheating$fluetype)
  #No information available about summer or winter efficiency waterheating
  the.summerefficiency <- rep("NULL", length(waterheating$uprn_new))
  the.winterefficiency <- rep("NULL", length(waterheating$uprn_new))
  the.cylinderfactoryinsulated <- 
    cylinder.factoryinsulated(waterheating$M30,waterheating$waterheatingsystemtype
                              ,waterheating$immersionheatertype
                              ,waterheating$spaceheatingsystemtype)
  the.cylinderinsulationthickness <- 
    cylinder.insulationthickness(waterheating$M31
                                 ,waterheating$waterheatingsystemtype
                                 ,waterheating$immersionheatertype
                                 ,waterheating$spaceheatingsystemtype)
  the.cylinderthermostatpresent <- 
    cylinder.thermostatpresent(waterheating$M32
                               ,waterheating$waterheatingsystemtype
                               ,waterheating$immersionheatertype
                               ,waterheating$spaceheatingsystemtype)
  the.cylindervolume <- cylinder.volume(waterheating$M29
                                        ,waterheating$waterheatingsystemtype
                                        ,waterheating$immersionheatertype
                                        ,waterheating$spaceheatingsystemtype)
  #No information available about installation year of waterheating
  the.installationyear <- rep("NULL", length(waterheating$uprn_new)) 
  the.mainheatingfuel <- wh.main.heatingfuel(waterheating$M18
                                             ,waterheating$spaceheatingmainfuel
                                             ,waterheating$withcentralheating)
  the.solarhotwaterpresent <- solar.hotwaterpresent(waterheating$D9)
  the.solarstoreincylinder <- solar.storeincylinder(the.solarhotwaterpresent
                                                    ,the.cylindervolume)
  the.solarstorevolume <- solar.storevolume(the.solarhotwaterpresent
                                            ,the.cylindervolume)
  the.withcentralheating <- with.centralheating(waterheating$withcentralheating)
  
  waterheating <- data.frame(aacode = waterheating$uprn_new
             ,basicefficiency = the.basicefficiency
             ,chpfraction = the.chpfraction
             ,communitychargingusagebased = the.communitychargingusagebased
             ,electrictariff = the.electrictariff
             ,fluetype = the.fluetype
             ,summerefficiency = the.summerefficiency
             ,winterefficiency = the.winterefficiency
             ,cylinderfactoryinsulated = the.cylinderfactoryinsulated
             ,cylinderinsulationthickness = the.cylinderinsulationthickness
             ,cylinderthermostatpresent = the.cylinderthermostatpresent
             ,cylindervolume = the.cylindervolume
             ,immersionheatertype = waterheating$immersionheatertype
             ,installationyear = the.installationyear
             ,mainheatingfuel = the.mainheatingfuel
             ,solarhotwaterpresent = the.solarhotwaterpresent
             ,solarstoreincylinder = the.solarstoreincylinder
             ,solarstorevolume = the.solarstorevolume
             ,waterheatingsystemtype = waterheating$waterheatingsystemtype
             ,withcentralheating = the.withcentralheating
    )

    waterheating$PcdbMatch <- rep(F,nrow(waterheating))
    waterheating <- join(waterheating, HAS_ELECTRIC_SHOWER(waterheating), by="aacode")
}

#'\pagebreak
#'
#'##Create waterheating
#'
#'The dataframe is created by importing the spaceheating file that was created
#' for the stock and matching that with waterheating systems from the waterheating
#' look up table, which has been created using a subset of information contained in 
#' the 'Table 4a: Heating systems (space and water)' in the SAP 2012 documentation.
#' 
#' 'lup-water-heating.csv' represents this information and is stored with the other 
#' lookup table csv files for heating systems.
#' 
#'@param shcs - the scottish survey data
#'
#'@param path.to.input - the path to the folder where the survey data and lookup
#' tables are placed
#'
#'@param path.to.outputs - the path to the output folder
create.waterheating <- function(shcs,path.to.output,path.to.input){
  #import the spaceheating data that was created by spaceheating.R
  spaceheating <- read.csv(file.path(path.to.output,"space-heating.csv")
                           , header=TRUE)
  #Rename the columns to allow the join onto the scottish survey without overwriting
  #required variables
  spaceheating <- data.frame(uprn_new = spaceheating$aacode
                             ,spaceheatingsystemtype = 
                               spaceheating$spaceheatingsystemtype
                             ,spaceheatingbasicefficiency = 
                               spaceheating$basicefficiency
                             ,spaceheatingmainfuel = spaceheating$mainheatingfuel
                             ,withcentralheating = 0)
  #Join the spaceheating dataframe to the scottish survey
  spaceheating <- join(spaceheating,shcs,by="uprn_new")
  #If the waterheating is flagged as from mains heating and the main heating type
  #is not a room heater or a storage heater then with central heating is set to 1.
  spaceheating$withcentralheating[spaceheating$M17=="Mains heating" &
                          (spaceheating$spaceheatingsystemtype != "storage_heater" &
                          spaceheating$spaceheatingsystemtype != "room_heater")] <- 1
  #If the main heating system is combi or cpsu it is assumed that waterheating comes
  #from the main heating system and the withcentralheating is set to 1.
  spaceheating$withcentralheating[spaceheating$spaceheatingsystemtype == "combi" |
                                  spaceheating$spaceheatingsystemtype == "cpsu"] <- 1
  #The waterheating look up file is imported
  waterheating <- read.csv(file.path(path.to.input,"lup-water-heating.csv")
                           , header=TRUE,na.strings = "")
  #Ensures that only stock where withcentralheating is false
  #have waterheating matched
  waterheating$withcentralheating <- 0

  matched.waterheating <- join(spaceheating,waterheating
                              ,by=c("M17","M18","withcentralheating"))
    
  return(matched.waterheating)
}

#' Adds a flaag to waterheating indicating whether an electric shower is present
#'
#' A simple lookup based on there being and instant single-point or mult-point waterheating source
#' that uses electricty as an indication of an elecrtic shower. No primary research has been carried out to
#' validate this method.
#'  
HAS_ELECTRIC_SHOWER <- function(waterheating){
    shower.detail <- data.frame(
        aacode = waterheating$aacode
        ,has.shower = waterheating$waterheatingsystemtype == "singlepoint" | waterheating$waterheatingsystemtype == "multipoint"
        ,shower.is.electric = waterheating$mainheatingfuel == "electricity"
    )
    shower.detail <- transmute(.data=shower.detail
      ,aacode
      ,hasElectricShower = has.shower & shower.is.electric)

    return (shower.detail)
}

#'
#'\pagebreak
#'
#'##Waterheating systems
#' 
#'With central heating
#' 
#'Values are mapped against true and false for the NHM, converting 1 to TRUE and 0 to
#' FALSE (as allocated above)
#' 
#'@param withcentralheating - flag created during create.waterheating
#' value is 0 if there is a separate water heating system to the spaceheating system
with.centralheating <- function(withcentralheating){
  withcentralheating <- as.factor(checked.revalue(
    as.factor(withcentralheating)
    ,c("0" = "FALSE"
       ,"1" = "TRUE"
    )))
  return(withcentralheating)
}

#'Main heating fuel (for water systems)
#' 
#'The water heating fuel is determined from the information cotained in the water 
#'heating fuel type variable in the SHCS unless the hot water is produced by the main
#' central heating.
#' 
#' NOte: community_heat is not a fuel type and it is assumed that for the significant
#' majority of community heating systems, mains_gas is the heating fuel.
#' 
#'@param wh.fuel - the heating fuel type of waterheating which comes from the lookup
#' table
#' 
#'@param sh.fuel - the heating fuel type from the spaceheating file
#' 
#'@param ch - flag indicating if the waterheating is provided by the spaceheating
#' 1 indicates waterheating is provided by spaceheating 
wh.main.heatingfuel <- function(wh.fuel, sh.fuel, ch){
  #waterheating fuel column is changed to the correct values for the NHM. 
  wh.fuel<-as.factor(checked.revalue(
    wh.fuel,
    c("Gas (mains)" = "mains_gas"
      ,"Bulk LPG"  = "bulk_lpg"
      ,"Bottled gas" = "bottled_lpg"
      ,"Oil" = "oil"
      ,"House coal" = "house_coal"
      ,"Smokeless fuel" = "house_coal"
      ,"Antracite nuts and grain" = "house_coal"
      ,"Wood chips" = "biomass_woodchip"
      ,"Wood logs" = "biomass_wood"
      ,"Wood pellets" = "biomass_pellets"
      ,"Peak electric" = "electricity"
      ,"Off-peak electric" = "electricity"
      ,"Communal heating, no CHP"  = "mains_gas"
      ,"Communal heating, with CHP" = "mains_gas"
      ,"Biogas" = "NULL"
      ,"Dual fuel" = "house_coal"
      ,"Other"  = "NULL"
      ,"Not applicable" = "NULL"
      ,"Unobtainable" = "NULL"
    )))
  #The two columns are then combined
  all.fuel <- ifelse(ch==1,"",levels(wh.fuel)[wh.fuel])
  return(all.fuel)
}

#'Basic efficiency
#' 
#'Basic efficiency is made from the basic efficiency of waterheating unless the
#' heating comes from central heating
#' 
#'@param wh.efficiency - the efficiency of waterheating which comes from the lookup
#' table
#' 
#'@param sh.efficiency - the efficiency of spaceheating which comes from spaceheating
#' file created by the spaceheating.R script
#' 
#'@param ch - flag indicating if the waterheating is provided by the spaceheating
#' 1 indicates waterheating is provided by spaceheating
wh.basic.efficiency <- function(wh.efficiency, sh.efficiency, ch){
  all.efficiency <- ifelse(ch==1,0,(wh.efficiency/100))
  return(all.efficiency)
}

#'Flue type
#' 
#'Flue type of the waterheating system is mapped to the correct values
#' 
#'@param flue - the flue type of the waterheating system from the waterheating lookup
#' table
wh.flue.type <- function(flue){
    as.factor(checked.revalue(
    flue
    ,c("notapplicable" = "notapplicable"
       ,"openflue"="openflue"
    )))
  return(flue)
}

#'
#'\pagebreak
#'
#'##Cylinder information
#' 
#'Cylinder factory insulated
#' 
#'Values are mapped against true and false for the NHM and checked for consistency
#' 
#'@param factory - M30 instalation type for hot water cylinder from SHCS
#'
#'@param w.heating - water heating system type
#' 
#'@param immersion - type of immersion heater (single/duel/null)
#'
#'@param s.heating - spaceheating system type
cylinder.factoryinsulated <- function(factory,w.heating,immersion,s.heating){
  factory <- as.factor(checked.revalue(
    factory,
    c("Sprayed" = "TRUE"
      ,"Jacket" = "FALSE"
      ,"Encapsulated" = "TRUE"
      ,"Both" =  "TRUE"
      ,"No Insulation" = "FALSE" 
      ,"No hw storage" = "NULL"
      ,"Unobtainable"  = "FALSE")))
  # These system types should not have cylinders
  factory[s.heating == "cpsu" | s.heating == "combi" | w.heating == "multipoint" 
          | w.heating == "singlepoint"] <- "NULL"
  #These system types should all have cylinders,
  #if no information assume no factory insulation
  factory[factory == "NULL" & (w.heating == "back_boiler" 
                               | w.heating == "standard_boiler" 
                               | immersion == "dual_coil" 
                               | immersion == "single_coil")] <- "FALSE" 
  return(factory)
}

#'Cylinder insulation thickness
#' 
#'Values are mapped against true and false for the NHM and checked for consistency
#' 
#'@param thickness - M31 cylinder insulation thickness from scottish survey
#'
#'@param w.heating - water heating system type
#' 
#'@param immersion - type of immersion heater (single/duel/null)
#'
#'@param s.heating - spaceheating system type
cylinder.insulationthickness <- function(thickness,w.heating,immersion,s.heating){
  thickness <- checked.renum(thickness,
                            data.frame(a = c(888,999), b = c(NA,NA)))
  # These system types should not have cylinders
  thickness[s.heating == "cpsu" | s.heating == "combi" | w.heating == "multipoint" 
            | w.heating == "singlepoint"] <- NA
  #These system types should all have cylinders,
  #if no information assume 0 insulation thickness
  thickness[is.na(thickness) == "TRUE" & (w.heating == "back_boiler" 
                                          | w.heating == "standard_boiler"
                                          | immersion == "dual_coil" 
                                          | immersion == "single_coil")] <- 0 
  return(thickness)
}

#'Cylinder thermostat present
#' 
#'Values are mapped against true and false for the NHM and checked for consistency
#' 
#'@param thermostat - M32 cylinder thermostat present from scottish survey
#'
#'@param w.heating - water heating system type
#' 
#'@param immersion - type of immersion heater (single/duel/null)
#'
#'@param s.heating - spaceheating system type
cylinder.thermostatpresent <- function(thermostat,w.heating,immersion,s.heating){
  thermostat <- as.factor(checked.revalue(
    thermostat
    ,c("Yes" = "TRUE"
       ,"No" = "FALSE"
       ,"Not applicable" = "NULL"
       ,"Unobtainable" = "FALSE")))
  # These system types should not have cylinders
  thermostat[s.heating == "cpsu" | s.heating == "combi" 
             | w.heating == "multipoint" | w.heating == "singlepoint"] <- "NULL"
  #These system types should all have cylinders, if no information assume
  #no thermostat
  thermostat[thermostat == "NULL" & (w.heating == "back_boiler" 
                                     | w.heating == "standard_boiler" 
                                     | immersion == "dual_coil" 
                                     | immersion == "single_coil")] <- "FALSE" 
  return(thermostat)
}

#'cylinder volume
#' 
#'Values are mapped against true and false for the NHM and checked for consistency, 
#'using information contained in the RD SAP documentation on typical sizes of 
#'cylinders.
#' 
#'@param volume - M29 cylinder volume from SHCS
#'
#'@param w.heating - water heating system type
#' 
#'@param immersion - type of immersion heater (single/duel/null)
#'
#'@param s.heating - spaceheating system type
cylinder.volume <- function(volume,w.heating,immersion,s.heating){
  volume <-as.factor(checked.revalue(
    volume
    ,c("Small (<90 L)" = "80" ##SAP 2009 table 2a value is 80
       ,"Normal (90-130 L)" = "110"
       ,"Medium (130-170 L)" = "140" ##SAP 2009 table 2a value is 140
       ,"Large (> 170 L)" = "210"
       ,"No hw storage" = "NULL"
       ,"Unobtainable" = "110" ##Assume normal size
    )))
  # These system types should not have cylinders
  volume[s.heating == "cpsu" | s.heating == "combi" | w.heating == "multipoint" 
         | w.heating == "singlepoint"] <- "NULL" 
  #These system types should all have cylinders, if they do not then assume 
  #normal size
    volume[volume == "NULL" & (w.heating == "back_boiler" 
                               | w.heating == "standard_boiler" 
                               | immersion == "dual_coil" 
                               | immersion == "single_coil")] <- "110" 
  return(volume)
}

#'
#' \pagebreak
#' 
#'##Community heating
#'
#'Set the chpfraction of chp heating systems
#'
#'When unknown, the default value for the fraction of heat optained from a CHP system
#'is 0.35.
#' 
#'@param wh.chp a vector containing the waterheating fuel type
#'
#'@param ch - flag indicating if the waterheating is provided by the spaceheating
#' 1 indicates waterheating is provided by spaceheating
wh.chp.fraction <- function(wh.chp,ch){
  wh.chp <- as.factor(checked.revalue(
    wh.chp,
    c("Gas (mains)" = "NULL"
      ,"Bulk LPG" = "NULL"
      ,"Bottled gas" = "NULL"
      ,"Oil" = "NULL"
      ,"House coal" = "NULL"
      ,"Smokeless fuel" = "NULL"
      ,"Antracite nuts and grain" = "NULL"
      ,"Wood chips" = "NULL"                
      ,"Wood logs" = "NULL"
      ,"Wood pellets" = "NULL"
      ,"Peak electric" = "NULL"
      ,"Off-peak electric" = "NULL"
      ,"Communal heating, no CHP" = "NULL"
      ,"Communal heating, with CHP" = "0.35"
      ,"Biogas" = "NULL"
      ,"Dual fuel" = "NULL"
      , "Other" = "NULL"
      ,"Not applicable" = "NULL"
      ,"Unobtainable" = "NULL"     
    )))
  wh.chp[ch==1] <- "NULL"
  return(wh.chp)
}

#'Community charging usage based
#'
#'This function sets 'communitychargingusagebased' to TRUE if a communal system. When 
#'unknown, it is assumed that charging for community systems is usage based.
#'#' 
#'@param wh.communal a vector containing the spaceheating fuel type from the survey
#'
#'@param ch - flag indicating if the waterheating is provided by the spaceheating
#' 1 indicates waterheating is provided by spaceheating
wh.community.chargingusagebased<- function(wh.communal,ch){
  wh.communal <- as.factor(checked.revalue(
    wh.communal,
    c("Gas (mains)" = "NULL"
      ,"Bulk LPG" = "NULL"
      ,"Bottled gas" = "NULL"
      ,"Oil" = "NULL"
      ,"House coal" = "NULL"
      ,"Smokeless fuel" = "NULL"
      ,"Antracite nuts and grain" = "NULL"
      ,"Wood chips" = "NULL"                
      ,"Wood logs" = "NULL"
      ,"Wood pellets" = "NULL"
      ,"Peak electric" = "NULL"
      ,"Off-peak electric" = "NULL"
      ,"Communal heating, no CHP" = "true"
      ,"Communal heating, with CHP" = "true"
      ,"Biogas" = "NULL"
      ,"Dual fuel" = "NULL"
      , "Other" = "NULL"
      ,"Not applicable" = "NULL"
      ,"Unobtainable" = "NULL"     
    )))
  wh.communal[ch==1] <- "NULL"
  return(wh.communal)
}

#'
#' \pagebreak
#' 
#'##Solar hot water heating
#'
#'Existance of solar hot water
#'
#'If an area bigger than zero of solar hot water panels exist assume that
#' case has solar hot water
#' 
#' @param solar - D9 (% roof with solar installed) from the scottish survey
solar.hotwaterpresent <- function(solar){
  #88: not applicable and 99: unobtainable
  solar[solar == 88 | solar == 99] <- NA 
  solar[solar > 0] <- "TRUE"
  solar[is.na(solar)=="TRUE"] <- "FALSE"
    return(solar)
}

#'Solar stored in cylinder
#'
#'If solar hot water exists and there is a cylinder for hot water then it is assumed
#' that all hot water is stored in the same cylinder as no other information is 
#' present in the scottish survey
#' 
#'@param solar - the.solarhotwaterpresent from the solar.howaterpresent function
#' 
#'@param volume - the volume of the hotwater cyclinder
solar.storeincylinder <- function(solar,volume){
  solar <- ifelse(solar=="TRUE" & volume != "NULL","TRUE","FALSE")
  return(solar)
}

#'Solar stored volume
#'
#'If solar hot water exists and there is a cylinder for hot water then it is assumed
#' that half the cylinder volume is used for storing hot water from the solar thermal
#' system. If there is no cylinder for the hot water system it is assumed that a
#' tank of volume 75 has been installed for the solar thermal system.
#' 
#'@param solar - the.solarhotwaterpresent from the solar.howaterpresent function
#' 
#'@param volume - the volume of the hotwater cyclinder
solar.storevolume <- function(solar,volume){
  solar <- ifelse(solar=="TRUE",as.numeric(levels(volume)[volume])/2,0)
  solar[is.na(solar)=="TRUE"] <- 75
  return(solar)
}
