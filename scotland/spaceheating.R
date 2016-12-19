#' ---
#' title: "Spaceheating functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save spaceheating data
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
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
save.spaceheating <- function(shcs, output, path.to.output) {
  write.csv(make.spaceheating(shcs,path.to.output), file=output, row.names=FALSE, na ="NULL")
}

#'Make a space-heating DTO from the SHCS
#' 
#'The survey does not contain all data required to create the space-heating dto; 
#' two additional sources of information are required: The sedbuk output
#' file (created for the Scottish stock) and the look-up tables created from SAP 2012
#' space heating system tables. The can be found in the SAP 2012 documentation in 
#' 'Table 4a: Heating systems (space and water)' and 'Table 4b: Seasonal efficiency 
#' for gas and oil boilers'.
#' 
#' Two variables - boiler manufacturer and model in the SHCS allow boiler matching to
#'  occur to the latest sedbuk database. This returns a series of information about 
#'  gas and oil boilers, including the type of boiler, whether it is condensing, if 
#'  it is a storage combi, what the storage volume is, and the fuel type. In some 
#'  instances the fuel type matched to the boiler description will be different to 
#'  fuel type described in the survey. Any information taken from SEDBUK is deemed to
#'  be correct.
#' 
#' A series of other variable describing the heating system are available in the SHCS
#' these are used to create a series of lookup tables in csv format which can be 
#' editted by the user. These assign the information available in SAP tables 4a and 
#' 4b where data matches are found.
#' 
#' teh sedbuk matching is performed first, and then all records which have not been 
#' successfully matched through sedbuk are matched to the SAP heating tables.
#' 
#' These sources of information are combined using the 
#' create.heating function to make the spaceheating dataframe.
#' 
#' @param shcs - the scottish survey data
make.spaceheating <- function(shcs, path.to.output) {
  
  #The spaceheating dataframe is first made. This is a combination of the scottish
  #survey data and information taken from sedbuk output and the heating table lookups
  spaceheating <- create.heating(shcs,path.to.output)

  the.basicefficiency <- basic.efficiency(spaceheating$basicefficiency)
  the.chpfraction <- chp.fraction(spaceheating$M5)
  the.communitychargingusagebased <- community.chargingusagebased(spaceheating$M5)
  the.electrictariff <- electric.tariff(spaceheating$L2,spaceheating$M2)
  the.fluetype <- flue.type(spaceheating$fluetype)
  the.winterefficiency <- winter.efficiency(spaceheating$winterefficiency)
  the.summerefficiency <- summer.efficiency(spaceheating$summerefficiency)
  the.iscondensing <- is.condensing(spaceheating$condensing)
  the.heatingsystemcontroltypes <- heating.systemcontroltypes(spaceheating$M21
                                                              ,spaceheating$M2)
  the.installationyear <- installation.year(spaceheating$M6
                                            ,spaceheating$installationyear)
  the.mainheatingfuel <- space.heatingfuel(spaceheating$checkedfueltype)
  the.secondaryheatingsystemtype <- secondary.heatingsystemtype(spaceheating$M21a
                                                                ,spaceheating$M22)
  the.spaceheatingsystemtype <- 
    space.heatingsystemtype(spaceheating$spaceheatingsystemtype)
  the.storageheatercontroltype <- storage.heatercontroltype(spaceheating$M21
                                                            ,spaceheating$M2)
  the.storageheatertype <- storage.heatertype(spaceheating$M16
                                             ,spaceheating$spaceheatingsystemtype)
  
  data.frame(aacode = spaceheating$uprn_new
             ,basicefficiency = the.basicefficiency
             ,chpfraction = the.chpfraction
             ,communitychargingusagebased = the.communitychargingusagebased
             ,electrictariff = the.electrictariff
             ,fluetype = the.fluetype
             ,summerefficiency = the.summerefficiency
             ,winterefficiency = the.winterefficiency
             ,iscondensing = the.iscondensing
             ,heatingsystemcontroltypes = the.heatingsystemcontroltypes
             ,installationyear = the.installationyear
             ,mainheatingfuel = the.mainheatingfuel
             ,secondaryheatingsystemtype = the.secondaryheatingsystemtype
             ,spaceheatingsystemtype = the.spaceheatingsystemtype
             ,isstoragecombicylinderfactoryinsulated = "NULL"
             ,storagecombicylinderinsulationthickness = 
               spaceheating$storeinsulationthickness
             ,isstoragecombicylinderthemostatpresent = "NULL"
             ,storagecombicylindervolume = spaceheating$storeboilervolume
             ,storagecombisolarvolume = spaceheating$storesolarvolume
             ,storageheatercontroltype = the.storageheatercontroltype 
             ,storageheatertype = the.storageheatertype
             ,PcdbMatch = spaceheating$PcdbMatch
  )
}


#' \pagebreak
#' 
#'##Create spaceheating
#'
#'This function creates a table of space heating information for each house case.
#' 
#'It requires the sedbuk-output.csv and lup-####.csv files to be in the same folder 
#' as the scottish stock.
#' 
#' The sedbuk matching is performed using a java programme built by CSE. For the 
#' scotland stock conversion the sedbuk matching has been previously performed to 
#' produce an output table (sedbuk-output.csv) which can be matched on urpn_new
#' 
#' Following that process, the SAP lookup tables are then used to catch the remaining
#' cases.
#' 
#' @param shcs - a dataframe containing all scottish survey data
create.heating <- function(shcs,path.to.output){
  #import sedbuk heating matches
  sedbuk <- sedbuk.heating(path.to.output)
  sedbuk.id <-data.frame(uprn_new=sedbuk$uprn_new)
  #match remaining cases against look up tables
  matched.cases <- heating.matching(shcs,path.to.output,sedbuk.id,
                                    "lup-boiler-4b.csv",c("M2","M5","M6","M7","M8"))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                     ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                     "lup-electric-boiler.csv",c("M2","M5","M9","M13")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                      ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                      "lup-gas-room-heater.csv",c("M2","M5","M14","M6")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                      ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                      "lup-heat-pump.csv",c("M2","M5","M11")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                      ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                      "lup-solid-fuel-boiler.csv",c("M2","M5","M9","M15")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                      ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                      "lup-storage-heater.csv",c("M2","M16")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                      ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                      "lup-warm-air.csv",c("M2","M5","M12","M6","M8")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                       ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                       "lup-oil-room-heater.csv",c("M2","M5","M15","M6")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                       ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                       "lup-other-room-heater.csv",c("M2","M5","M15")))
  matched.cases <- rbind(matched.cases,heating.matching(shcs,path.to.output
                        ,rbind(sedbuk.id,data.frame(uprn_new=matched.cases$uprn_new)),
                        "lup-community-heating.csv",c("M2","M5")))
  
  #combine all spaceheating with the main table
  matched.sedbuk <- data.frame(uprn_new=sedbuk$uprn_new
                               ,basicefficiency=sedbuk$annualefficiency
                               ,winterefficiency=sedbuk$winterefficiency
                               ,summerefficiency=sedbuk$summerefficiency
                               ,spaceheatingsystemtype=sedbuk$boilertype
                               ,fluetype=sedbuk$fluetype
                               ,checkedfueltype=sedbuk$fueltype
                               ,condensing=sedbuk$condensing
                               ,installationyear=NA
                               ,PcdbMatch=sedbuk$PcdbMatch
  )
  
  matched.cases <-data.frame(uprn_new=matched.cases$uprn_new
                             ,basicefficiency=matched.cases$basicefficiency
                             ,winterefficiency=matched.cases$winterefficiency
                             ,summerefficiency=matched.cases$summerefficiency
                             ,spaceheatingsystemtype=
                               matched.cases$spaceheatingsystemtype
                             ,fluetype=matched.cases$fluetype
                             ,checkedfueltype=matched.cases$mainheatingfuel
                             ,condensing=as.factor(matched.cases$iscondensing)
                             ,installationyear=matched.cases$installationyear
                             ,PcdbMatch=rep(F, nrow(matched.cases)) 
  )
  
  matched.heating <- rbind(matched.sedbuk,matched.cases)
  sedbuk<-subset(sedbuk,select=c("uprn_new","storeboilervolume"
                                 ,"storesolarvolume","storeinsulationthickness"))
  matched.heating <- join(matched.heating,sedbuk,by="uprn_new")
  heating <- join(shcs,matched.heating,by="uprn_new")
  
  return(heating)
}
#'
#' \pagebreak
#' 
#'###Spaceheating import files
#'
#'This function imports data from the sedbuk-output (sedbuk-output.csv) file.
#' 
#'This file must be available in the same folder as the scottish stock. If a 
#'different file name is use then it must be corrected in the creation of the sedbuk 
#'data command below.
#' 
#'@param none
sedbuk.heating <- function(path.to.output){
  sedbuk <- read.csv(file.path(path.to.output,"sedbuk-output.csv"), header=TRUE)
  colnames(sedbuk)[names(sedbuk) == "aacode"] <- "uprn_new"
  sedbuk<-subset(sedbuk,select=c(-manufacturer,-brand,-model,-qualifier))
  sedbuk["PcdbMatch"] <- rep(T, nrow(sedbuk)) 
  return(sedbuk)
}

#'And once matching with sedbuyk data is complete, this function imports data from 
#'the SAP 4a and 4b look up tables...
#' 
#'These file must be available in the same folder as the scottish stock
#'  
#'@param shcs - a dataframe containing all survey data
#' 
#'@param matched.id - a vector containing the uprn_new/aacodes of all houses for
#' which space-heating information has yet to be matched
#' 
#'@param name.to.heatinglookup - a character string which contains the name of the 
#' look-up file including it's extention (.csv)
#' 
#'@param columns.to.match - a character list of the column names that appear at the 
#'start of each look-up file (i.e c("M2","M5"))
#'
heating.matching <- function(shcs,path.to.output,matched.id
                             ,name.to.heatinglookup,columns.to.match){
  boiler <- read.csv(file.path(path.to.output,name.to.heatinglookup), header=TRUE)
  if (("installationyear" %in% colnames(boiler))==FALSE){
    boiler$installationyear <- NA
  }
  if (("winterefficiency" %in% colnames(boiler))==FALSE){
    boiler$winterefficiency <- NA
  }
  if (("summerefficiency" %in% colnames(boiler))==FALSE){
    boiler$summerefficiency <- NA
  }
  
  unmatched <- subset(shcs,!shcs$uprn_new %in% matched.id$uprn_new
                      ,select=c("uprn_new",columns.to.match))
  unmatched<-join(unmatched,boiler,by=columns.to.match)
  matched<-subset(unmatched,is.na(unmatched$Heating.System.Type)==FALSE
                  ,select=c(uprn_new, winterefficiency, summerefficiency
                            ,basicefficiency, spaceheatingsystemtype, fluetype
                            ,mainheatingfuel, iscondensing,installationyear))
   return(matched)
}

#'
#' \pagebreak
#' 
#'##Efficiency
#'
#'This function turns the basic efficiency from a percentage to a decimal as
#' required for the dto files
#' 
#'@param efficiency - a vector containing the basic efficiency of spaceheating
basic.efficiency <- function(efficiency){
  efficiency = efficiency/100
  return(efficiency)
}

#'This function turns the winter efficiency from a percentage to a decimal as 
#' required for the dto files
#' 
#'@param efficiency - a vector containing the winter efficiency of spaceheating
winter.efficiency <- function(efficiency){
  efficiency = efficiency/100
  return(efficiency)
}

#'This function turns the summer efficiency from a percentage to a decimal as
#' required for the dto files
#' 
#'@param efficiency - a vector containing the summer efficiency of spaceheating
summer.efficiency <- function(efficiency){
  efficiency = efficiency/100
  return(efficiency)
}

#'
#' \pagebreak
#' 
#'##Heating system type
#'
#'Relabel the spaceheatingsystemtype to the correct formats for the NHM stock import
#'
#'@param heatingsystem - a vector containing the heating system type from sedbuk and
#' look-up data files
space.heatingsystemtype <- function(heatingsystem){
  heatingsystem <- as.factor(checked.revalue(
    heatingsystem,
    c("CPSU" = "cpsu"
      ,"INSTANT_COMBI" = "combi"
      ,"REGULAR" = "standard"
      ,"STORAGE_COMBI" = "storage_combi"              
      ,"back_boiler" = "back_boiler"
      ,"combi" = "combi"
      ,"cpsu" = "cpsu"
      ,"standard" = "standard"                    
      ,"room_heater" = "room_heater"
      ,"storage_heater" = "storage_heater"
      ,"warm_air" = "warm_air"
      ,"community_heating_with_chp" = "community_heating_with_chp"
      ,"community_heating_without_chp" = "community_heating_without_chp"
      ,"ground_source_heat_pump" = "ground_source_heat_pump"
      ,"air_source_heat_pump" = "air_source_heat_pump"
    )))
  return(heatingsystem)
}

#'This function relabels elements of a vector to the correct versions required for
#' the dto files
#' 
#' NOte: community_heat is not a fuel type and it is assumed that for the significant
#' majority of community heating systems, mains_gas is the heating fuel.
#' 
#'@param fuel - a vector containing the main heating fuel type associated with the 
#' space heater
space.heatingfuel <- function(fuel){
  fuel <- as.factor(checked.revalue(
    fuel,
    c("MAINS_GAS" = "mains_gas"
      ,"mains_gas" = "mains_gas"
      ,"OIL" = "oil"
      ,"oil" = "oil"
      ,"bottled_lpg" = "bottled_lpg"
      ,"bulk_lpg" = "bulk_lpg"
      ,"biomass_pellets" = "biomass_pellets"
      ,"biomass_wood" = "biomass_wood"
      ,"biomass_woodchip" = "biomass_woodchip"
      ,"house_coal" = "house_coal"          
      ,"electricity" = "electricity"
      ,"community_heat" = "mains_gas"
    )))
  return(fuel)
}

#'This function re-labels elements of a vector to the correct versions required for
#' the dto files
#' 
#'@param flue - a vector containing the flue type associated with the space heater
flue.type <- function(flue){
  flue <- as.factor(checked.revalue(
    flue,
    c( "BalancedFlue" = "balancedflue"
       ,"FanAssistedBalancedFlue" = "fanassistedbalancedflue"
       ,"OpenFlue" = "openflue"
       ,"balancedflue" = "balancedflue"           
       ,"fanassistedbalancedflue" = "fanassistedbalancedflue"
       ,"notapplicable" = "notapplicable"
       ,"openflue" = "openflue"
       ,"chimney" = "chimney"
    )))
    return(flue)
}

#'This function relables elements of a vector to the correct versions required for
#' the dto files
#' 
#'@param condensing - a vector containing a flag to indicate wether each boiler is
#' condensing or not, information from sedbuk output and look-up files
is.condensing <- function(condensing){
  return(condensing)
}

#'This function assigns an installation year
#' 
#'The installation year is assigned for the main space heating system from
#' the information in the look-up files as a priority, if this is not available 
#' then installation year is determined from the bands provided in the survey
#' 
#'@param heatyear a - vector containing the band in which the main space heating 
#' system was installed from the survey (M6)
#'
#'@param checkedheatyear - a vector containing the year in which the main space 
#' heating system was installed from the look-up files
installation.year <- function(heatyear,checkedheatyear){
  heatyear <- as.factor(checked.revalue(
    as.factor(heatyear),
    c("1998+" = "2005"
      ,"pre 1998" = "1995"
      ,"old system" = "1985"
      ,"Other" = "9999" 
      ,"Not applicable" = "9999" 
      ,"Unobtainable" = "9999"
    )))
  heatyear<-as.numeric(levels(heatyear))[heatyear]
  heatyear[heatyear==9999]<-NA
  checkedheatyear<-ifelse(is.na(checkedheatyear)==FALSE,checkedheatyear,heatyear)
  return(checkedheatyear)
}

#'
#' \pagebreak
#' 
#'##Secondary heating system
#'
#'Function creates the secondary heating system type - first a check is done
#'on existance of secondary heating system, then if a secondary system exists, the 
#'data describing secondary heating system type and fuel is used to assigned a 
#'secondary heater type from the room heating information in SAP table 4a.
#'
#'@param secondarysystem - a vector indicating existance of a secondary heating
#' system taken from the survey (M21a)
#'@param roomheaters - a vector indicating the type of room heater that exists
#' taken from the survey (M22)
secondary.heatingsystemtype <- function(secondarysystem,roomheaters){
  secondarysystem<-as.factor(ifelse(secondarysystem=="Yes"
                                    ,as.character(roomheaters),
                                    as.character(secondarysystem)))
  secondarysystem <- as.factor(checked.revalue(
    secondarysystem,
    c("Closed solid fuel fire" = "gas_fire"
      ,"Elec room heaters" = "electric_room_heaters"
      ,"Gas,coal effect fire" = "gas_coal_effect_fire"
      ,"No" = "no_secondary_system"
      ,"No other room heaters" = "no_secondary_system"
      ,"Not applicable" = "no_secondary_system"        
      ,"Open solid fuel fire" = "open_fire"
      ,"Other" = "electric_room_heaters"
      ,"Post 1980 gas fire" = "gas_fire_flueless"
      ,"Pre 1980 gas fire" = "gas_fire_open_flue"
      ,"Unobtainable" = "not_known"
    )))
  secondarysystem[is.na(secondarysystem)==TRUE] <- "no_secondary_system"
  return(secondarysystem)
}

#'
#' \pagebreak
#' 
#'##Heating system controls
#'
#'This function sets the heating system control types to the correct format for the
#' dto files and ensures that the stock cannot have any system control types that are 
#' not possible options for the main space-heating system. 
#' 
#' in the dto, the heating control variable is a list, separated by semi-colons, of 
#' all heating control systems present in the dwelling.
#' 
#'@param controls - a vector of controls from the survey
#' 
#'@param heatingsystem - a vector of heating system types from the survey
heating.systemcontroltypes <- function(controls,heatingsystem){
  #Storage heating controls are placed in a separate column and are left blank
  #in this function
  controls <- as.factor(checked.revalue(
    controls,
    c("No controls" = ""                
      ,"Programmer only" = "programmer"          
      ,"Room stat only" = "roomthermostat"
      ,"Programmer & room stat" = "programmer;roomthermostat"
      ,"Programmer, room stat & TRV" = 
        "programmer;roomthermostat;thermostaticradiatorvalve"
      ,"Programmer & TRV" = "programmer;thermostaticradiatorvalve"
      ,"Boiler manager" = "boilerenergymanager" #boiler
      ,"TRV only" = "thermostaticradiatorvalve"                   
      ,"appliance stat" = "appliancethermostat" #room heaters
      ,"appliance stat & prog" = "programmer;appliancethermostat"
      ,"manual charge control" = ""
      ,"auto charge control" = ""       
      ,"More than 1 stat" = "roomthermostat"
      ,"Time / temp zone control" = "timetemperaturezonecontrol"
      ,"Programmer, 2 stats" = "programmer;roomthermostat"
      ,"Other" = ""                     
      ,"No Controls" = "" 
      ,"Unobtainable"  = ""         
    )))
  #consistency checks
  #Storage heating controls are stored in a separate column
  #(storageheatercontroltype)
  controls[heatingsystem=="Storage heating"]<-""
  #Only room heaters can have appliance thermostats
  controls[(heatingsystem!="Room heater" & 
              heatingsystem != "Room heater (bb no rads)") & 
              (controls=="appliancethermostat" | 
                 controls == "programmer;appliancethermostat")]<- ""
  #only boilers can have these types of controls
  controls[heatingsystem!="Boiler" & controls=="boilerenergymanager"]<- ""

  return(controls)
}

#'
#' \pagebreak
#' 
#'##Storage heating
#'
#'Function creates the controls for storage heaters
#'
#'This function sets the storage heating system control types to the correct format 
#' for the dto files and ensures that stock cannot have any system control types
#' that are not possible options for the storage heating systems.
#' 
#'@param controls - a vector of controls from the survey
#'
#'@param heatingsystem - a vector of heating system types from the survey
storage.heatercontroltype <- function(controls,heatingsystem){
  controls <- as.factor(checked.revalue(
    controls,
    c("No controls" = "NULL"                
      ,"Programmer only" = "NULL"          
      ,"Room stat only" = "NULL"
      ,"Programmer & room stat" = "NULL"
      ,"Programmer, room stat & TRV" = "NULL"
      ,"Programmer & TRV" = "NULL"
      ,"Boiler manager" = "NULL"
      ,"TRV only" = "NULL"                   
      ,"appliance stat" = "NULL"
      ,"appliance stat & prog" = "NULL"
      ,"manual charge control" = "manualchargecontrol"
      ,"auto charge control" = "automaticchargecontrol"        
      ,"More than 1 stat" = "NULL"
      ,"Time / temp zone control" = "NULL"
      ,"Programmer, 2 stats" = "NULL"
      ,"Other" = "NULL"                     
      ,"No Controls" = "NULL" 
      ,"Unobtainable"  = "NULL"         
    )))
  #only storage heaters control types are recorded in this column, set to null in all
  #other cases.
  controls[heatingsystem!="Storage heating"]<-"NULL" 
  return(controls)
}

#'Map the type of storage heater
#'
#'Function creates the storage heater type by re-labeling storageheater to required
#' values for the NHM stock import, it then ensures that only heating systems of 
#' the type storage heater have a storage heater type.
#'
#'@param storageheater - vector containing information about the type of storage
#' heater taken from the survey (M16)
storage.heatertype <- function(storageheater,heatingsystem){
  storageheater <- as.factor(checked.revalue(
    storageheater,
    c("New style" = "slimline"
      ,"Fan Assisted" = "fan"
      ,"Old Style" = "oldlargevolume"
      ,"Integrated storage / direct" = "integrateddirectacting"
      ,"Under Floor" = "oldlargevolume" #override as not an option in the NHM
      ,"Not applicable" = "NULL"
      ,"Unobtainable" = "NULL"               
    )))
  storageheater<-as.factor(ifelse(heatingsystem=="storage_heater"
                                  ,as.character(storageheater)
                                  ,"NULL"))
  return(storageheater)
}

#'
#' \pagebreak
#' 
#'##Community heating
#'
#'This function sets the chpfraction of chp heating systems. When unknown, the 
#'default value for the fraction of heat optained from a CHP system is 0.35.
#' 
#'@param chp a vector containing the spaceheating fuel type from the survey
chp.fraction <- function(chp){
  chp <- as.factor(checked.revalue(
    chp,
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
  return(chp)
}

#'This function sets communitychargingusagebased to true if a communal system. When 
#'unknown, it is assumed that charging for community systems is usage based.
#'
#'The default value 
#' 
#'@param communal a vector containing the spaceheating fuel type from the survey
community.chargingusagebased <- function(communal){
  communal <- as.factor(checked.revalue(
    communal,
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
  return(communal)
}
