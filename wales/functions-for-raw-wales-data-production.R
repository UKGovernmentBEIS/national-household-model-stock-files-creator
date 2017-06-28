
#' ##################################################################################
#'  
#'  THIS IS THE KEY FUNCTION TO ALTER IF WISHING TO CHANGE THE ORDER OF VARIABLES AND
#'  THEREFORE THE PRIORITY ASSIGNED TO THEM IN THE REWEIGHTING PROGRAMME AND STOCK 
#'  CREATION
#'  
#'  The higher the variable is up the list below then the more priority given to 
#'  matching it with the original data
#'  
#' ##################################################################################

create.wales.megacrosstab <- function(liw.data) 
{
  ddply(liw.data, .(builtformtype,
                    mainheatingfuel,
                    buildyear,
                    morphologytype,
                    tenuretype,
                    numofbedrooms,
                    externalwallconstructiontype,
                    insulationthickness,
                    householdincomebeforetax,
                    children,
                    chiefincomeearnersage,
                    hasdisabledorsickoccupant
                    #numofhabitalrooms, #not included
                    #cavitywallinsulation, #not included as effiency levels will have changes and reweighting model should account for this (otherwise forcing stock to reproduce caity wall insulation levels of 2008)
                    #adults, #not included
  ),
  summarise, 
  weight = sum(GR2))
}

#' ##################################################################################

##OTHER KEY FUNCTIONS REQUIRED TO PRODUCE WALES DATA

produce.wales.data <- function(path.to.wales) {
      
  ##get liw data
  liw.data <- get.data(path.to.wales)
  
  #recode variables
  liw.data <- recode.new.variables(liw.data)
      
  #add wall type
  liw.data <- join.wall.type(liw.data, path.to.wales)
  
  #uprate incomes
  liw.data <- income.uprating(liw.data)
  
  #select key variables
  liw.data <- select.key.variables(liw.data)
  
  #produced weighted summary of data
  weighted.wales.data <- create.wales.megacrosstab(liw.data)
    
  return(weighted.wales.data)
  
}


get.data <- function(path.to.wales) {
  LOAD <- function(name) read.spss(file.path(path.to.wales, name), to.data.frame = TRUE)
  #import fuel poverty data
  fuel.poverty <- LOAD("sss20854_101008_v1_liw_ps_2008_fuel_poverty.sav")
    
  #import derived physical dataset
  derived <- LOAD("sss20860_100216_v1_liw_ps_2008_derived.sav")
    
  #correct ID to lower case
  names(derived)[names(derived) == "ADDNO"] <- "addno"
  
  interior <- LOAD("sss20860_100216_v1_liw_ps_2008_interior.sav")
  names(interior)[names(interior) == "ADDNO"] <- "addno"
    
  shape <- LOAD("sss20860_100216_v1_liw_ps_2008_shape.sav")
  names(shape)[names(shape) == "ADDNO"] <- "addno"
    
  firstimp <- LOAD("sss20860_100216_v1_liw_ps_2008_firstimp.sav")
  names(firstimp)[names(firstimp) == "ADDNO"] <- "addno"
    
  loft <- LOAD("sss20860_100216_v1_liw_ps_2008_loft.sav")
  names(loft)[names(loft) == "ADDNO"] <- "addno"
    
  elevate <- LOAD("sss20860_100216_v1_liw_ps_2008_elevate.sav")
  names(elevate)[names(elevate) == "ADDNO"] <- "addno"
    
  elevate <- subset(elevate, select = c("addno", "FELCAVFF", "FELCAVLF", "FELCAVRF", "FELCAVBF"))
  elevate$cavitywallinsulation <- ifelse(elevate$FELCAVFF == "Yes" | elevate$FELCAVLF == "Yes" | elevate$FELCAVRF == "Yes" | elevate$FELCAVBF == "Yes",
                                         TRUE,
                                         FALSE)
   
  
  elevate$cavitywallinsulation <- ifelse(is.na(elevate$cavitywallinsulation), FALSE, elevate$cavitywallinsulation)
  
  
  ## this file does not read correctly with read.spss;
  ## however, it does work OK with spss.system.file from memisc
  services <-
      as.data.table(
          as.data.set(
              spss.system.file(file.path(path.to.wales,
                                         "sss20860_100216_v1_liw_ps_2008_services.sav"))))
    
  household.data <- LOAD("household_file_2008_with_disclosure_control.sav")
  household.data <- subset(household.data, select = c("addno", "h2", "h6", "p2", "p5du"))
  household.data$hrpsex <- household.data$p2
  household.data$p2 <- NULL

  print("Doing person data")
  #person.data
  person.data <- LOAD("person_file_2008_with_disclosure_control.sav")
  person.data$longtermillnessdisability <- ifelse(person.data$p10 == "Yes", 1, 0)
  person.data$longtermillnessdisability <- ifelse(is.na(person.data$longtermillnessdisability), 0, person.data$longtermillnessdisability)
  colnames(person.data)[colnames(person.data) == "ADDNO"] <- "addno"
  person.data <- ddply(person.data, .(addno), summarise, longtermillnessdisability = max(longtermillnessdisability))
  
  #join derived to fuelpov data to start main dataset
  liw.data <- join(fuel.poverty, derived, by = "addno")
  
  liw.data <- join(liw.data, interior, by = "addno")
  
  liw.data <- join(liw.data, shape, by = "addno")
  
  liw.data <- join(liw.data, firstimp, by = "addno")
  
  liw.data <- join(liw.data, loft, by = "addno")
  
  liw.data <- join(liw.data, household.data, by = "addno")
  
  liw.data <- join(liw.data, person.data, by = "addno")
  
  liw.data <- join(liw.data, elevate, by = "addno")

  liw.data <- join(liw.data, services, by="addno")
  
  liw.data <- subset(liw.data, select = c("addno",
                                          "GR2",
                                          "hv21r1",
                                          "FODCONSA",
                                          "FODDTYPE",
                                          "hv16r2",
                                          "av6a",
                                          "FINROOMS",
                                          "hv21r1",
                                          "FMTCONST",
                                          "hv31",
                                          "fpfullinc",
                                          "FLITHICK",
                                          "FDHMFLRS",
                                          "h2",
                                          "h6",
                                          "hrpsex",
                                          "p5du",
                                          "longtermillnessdisability",
                                          "cavitywallinsulation",
                                          "finchtyp"))
  
  return(liw.data)
}


print("doingWallTypes")
#walltypes
determine.predominant.walltype <- function(path.to.wales) {
  #get shape data for contruction type
  shape <- read.spss(file.path(path.to.wales, "sss20860_100216_v1_liw_ps_2008_shape.sav"), to.data.frame = TRUE)
  names(shape)[names(shape) == "ADDNO"] <- "addno"
  shape <- subset(shape, select = c("addno", "FMTCONST"))
  
  wall.structure <- read.spss(file.path(path.to.wales, "sss20860_100216_v1_liw_ps_2008_wallstru.sav"), to.data.frame = TRUE)
  
  
  front.wall.type <- subset(wall.structure, select = c("addno", "FEXWSTYPE", "FEXWS1TE"))
  back.wall.type <- subset(wall.structure, select = c("addno", "FEXWSTYPE", "FEXWS2TE"))  
  
  #determine the predominant wall type of the front and back face
  front.wall.type$front.predominant.wall.type <- ifelse(front.wall.type$FEXWS1TE >= 6, levels(front.wall.type$FEXWSTYPE)[front.wall.type$FEXWSTYPE], "")
  back.wall.type$back.predominant.wall.type <- ifelse(back.wall.type$FEXWS2TE >= 6, levels(back.wall.type$FEXWSTYPE)[back.wall.type$FEXWSTYPE], "")
  
  #select each predominant wall type into separate table and then join
  front.wall.type <- subset(front.wall.type, front.predominant.wall.type != "")
  back.wall.type <- subset(back.wall.type, back.predominant.wall.type != "")
  
  wall.type <- join(front.wall.type, back.wall.type, by = "addno", type = "left")
  
  #if back wall type missing then set to front wall type
  wall.type$back.predominant.wall.type <- ifelse(is.na(wall.type$back.predominant.wall.type),
                                                 wall.type$front.predominant.wall.type,
                                                 wall.type$back.predominant.wall.type)
 
   #join data from shape file construction type
  wall.type <- join(wall.type, shape, by = "addno", type = "right")
 #determine whether back and front are the same
  wall.type$same.types <- wall.type$front.predominant.wall.type == wall.type$back.predominant.wall.type
  #recode set the wall type to be the front wall type where back and front are the same.
  wall.type$predominant.wall.type <- ifelse(wall.type$same.types, wall.type$front.predominant.wall.type, NA)
  
  #recode final wall type for these values
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == ">9 inch solid"] <- "sandstone"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "9 inch solid"] <- "graniteorwhinstone"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "Concrete panels"] <- "systembuild"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "In situ concrete"] <- "systembuild"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "Masonry cavity"] <- "cavity"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "Masonry single leaf"] <- "solidbrick"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "Metal sheet"] <- "metalframe"
  wall.type$externalwallconstructiontype[wall.type$predominant.wall.type == "Timber panels"] <- "timberframe"
  
  #select subset of final wall type values where front and back agree
  single.walls <- subset(wall.type, select = c("addno", "externalwallconstructiontype"), !is.na(externalwallconstructiontype))
  
  #select entries where multiple wall types are specified
  double.walls <- subset(wall.type, is.na(externalwallconstructiontype))
  
  #recode FMT into NHM coding
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Masonry/Boxwall/Solid"] <- "solidbrick"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Masonry/Boxwall/Cavity"] <- "cavity"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Masonry/Crosswall"] <- "solidbrick"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Boxwall/In-situ"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Boxwall/Precast <1m"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Boxwall/Precast >1m"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Crosswall/In-situ"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Crosswall/Precast panel"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Frame/In-situ"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Concrete/Frame/Precast"] <- "systembuild"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Timber/Frame/Pre 1919"] <- "timberframe"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Timber/Frame/Post 1919"] <- "timberframe"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Metal/Frame"] <- "metalframe"
  double.walls$FMTCONST_recode[double.walls$FMTCONST == "Other"] <- "systembuild"
    
  #where wall area of front is greater than the back wall area then choose front wall type and vice versa
  double.walls$predominant.wall.type <- ifelse(double.walls$FEXWS1TE >= double.walls$FEXWS2TE, double.walls$front.predominant.wall.type,
                                            ifelse(double.walls$FEXWS1TE < double.walls$FEXWS2TE, double.walls$back.predominant.wall.type, NA))
  
  #recode final wall type for these values
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == ">9 inch solid"] <- "sandstone"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "9 inch solid"] <- "graniteorwhinstone"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "Concrete panels"] <- "systembuild"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "In situ concrete"] <- "systembuild"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "Masonry cavity"] <- "cavity"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "Masonry single leaf"] <- "solidbrick"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "Metal sheet"] <- "metalframe"
  double.walls$externalwallconstructiontype[double.walls$predominant.wall.type == "Timber panels"] <- "timberframe"
  
  #where these values are null, set to the FMT recoded value
  double.walls$externalwallconstructiontype <- ifelse(is.na(double.walls$externalwallconstructiontype), 
                                                      double.walls$FMTCONST_recode, 
                                                      double.walls$externalwallconstructiontype)
 
  double.walls <- subset(double.walls, select = c("addno", "externalwallconstructiontype"))
  
  #bring single and double frames together
  wall.type <- rbind(single.walls, double.walls)
  
  #check for nulls
  #table(wall.type$externalwallconstructiontype, useNA = "ifany")
   
  return(wall.type)
  
}

join.wall.type <- function(liw.data, path.to.wales) {
  
  wall.type <- determine.predominant.walltype(path.to.wales)
  liw.data <- join(liw.data, wall.type, by = "addno")
  return(liw.data)

}


recode.new.variables <- function(liw.data) {
  
  #building type
  liw.data$builtformtype[liw.data$FODDTYPE == 1] <- "endterrace"
  liw.data$builtformtype[liw.data$FODDTYPE == 2] <- "midterrace"
  liw.data$builtformtype[liw.data$FODDTYPE == 3] <- "semidetached"
  liw.data$builtformtype[liw.data$FODDTYPE == 4] <- "detached"
  liw.data$builtformtype[liw.data$FODDTYPE == 5] <- "convertedflat"
  liw.data$builtformtype[liw.data$FODDTYPE == 6 & liw.data$FDHMFLRS < 5] <- "purposebuiltlowriseflat"
  liw.data$builtformtype[liw.data$FODDTYPE == 6 & liw.data$FDHMFLRS >= 5] <- "purposebuilthighriseflat"
  liw.data$builtformtype[liw.data$FODDTYPE == 7] <- "convertedflat"
  liw.data$builtformtype[liw.data$FODDTYPE == 8] <- "convertedflat"
  liw.data$builtformtype <- as.factor(liw.data$builtformtype)
  
  #externalwallconstructiontype (replaced with new function)
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Masonry/Boxwall/Solid"] <- "solidbrick"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Masonry/Boxwall/Cavity"] <- "cavity"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Masonry/Crosswall"] <- "solidbrick"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Boxwall/In-situ"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Boxwall/Precast <1m"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Boxwall/Precast >1m"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Crosswall/In-situ"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Crosswall/Precast panel"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Frame/In-situ"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Concrete/Frame/Precast"] <- "systembuild"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Timber/Frame/Pre 1919"] <- "timberframe"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Timber/Frame/Post 1919"] <- "timberframe"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Metal/Frame"] <- "metalframe"
  #liw.data$externalwallconstructiontype[liw.data$FMTCONST == "Other"] <- "systembuild"
  
  #tenure
  liw.data$tenuretype[liw.data$hv21r1 == "Owner-occupied"] <- "owneroccupied"
  liw.data$tenuretype[liw.data$hv21r1 == "Local authority"] <- "localauthority"
  liw.data$tenuretype[liw.data$hv21r1 == "Housing Association"] <- "housingassociation"
  liw.data$tenuretype[liw.data$hv21r1 == "Private rented"] <- "privaterented"
  liw.data$tenuretype <- as.factor(liw.data$tenuretype)
  
  #rural urban classification
  liw.data$morphologytype[liw.data$av6a == "Urban >10k"] <- "urban"
  liw.data$morphologytype[liw.data$av6a == "Town and fringe"] <- "townandfringe"
  liw.data$morphologytype[liw.data$av6a == "Village"] <- "village"
  liw.data$morphologytype[liw.data$av6a == "Hamlet & isolated dwellings"] <- "hamletsandisolateddwellings"
  liw.data$morphologytype <- as.factor(liw.data$morphologytype)
  
  #hasdisabledorsickoccupant
  #liw.data$hasdisabledorsickoccupant <- liw.data$p10
  liw.data$hasdisabledorsickoccupant <- ifelse(liw.data$longtermillnessdisability == 1, TRUE, FALSE)
  
  #numeric values
  liw.data$adults <- as.numeric(liw.data$h2) - as.numeric(liw.data$hv16r2)
  liw.data$children <- liw.data$hv16r2
  liw.data$buildyear <- liw.data$FODCONSA
  liw.data$householdincomebeforetax <- liw.data$fpfullinc
  liw.data$numofbedrooms <- liw.data$h6
  liw.data$numofhabitalrooms <- liw.data$FINROOMS
  liw.data$chiefincomeearnersage <- liw.data$hv31
  liw.data$HRP.age <- liw.data$p2
  
  liw.data$insulationthickness[liw.data$FLITHICK == "Non insulation"] <- 0
  liw.data$insulationthickness[liw.data$FLITHICK == "25mm"] <- 25
  liw.data$insulationthickness[liw.data$FLITHICK == "50mm"] <- 50
  liw.data$insulationthickness[liw.data$FLITHICK == "75mm"] <- 75
  liw.data$insulationthickness[liw.data$FLITHICK == "100mm"] <- 100
  liw.data$insulationthickness[liw.data$FLITHICK == "125mm"] <- 125
  liw.data$insulationthickness[liw.data$FLITHICK == "150mm"] <- 150
  liw.data$insulationthickness[liw.data$FLITHICK == "200mm"] <- 200
  liw.data$insulationthickness[liw.data$FLITHICK == "250mm"] <- 250
  liw.data$insulationthickness[liw.data$FLITHICK == "300mm"] <- 300
  liw.data$insulationthickness[liw.data$FLITHICK == ">300mm"] <- 350

  liw.data$mainheatingfuel <-
        mapvalues(as.factor(liw.data$finchtyp),
                  from=c("Mains gas",
                         "Bulk LPG gas",
                         "Bottled gas",
                         "Oil",
                         "Coal",
                         "Smokeless fuel",
                         "Anthracite",
                         "Wood",
                         "Standard electricity",
                         "7hr tariff electricity",
                         "10hr tariff electricity",
                         "24hr tariff electricity",
                         "CHP/ Waste heat communal",
                         "From communal boiler",
                         "Biomass",
                         "Other renewable"),
                  to=c("MAINS_GAS",
                       "BULK_LPG",
                       "BOTTLED_LPG",
                       "OIL",
                       "HOUSE_COAL",
                       "HOUSE_COAL",
                       "HOUSE_COAL",
                       "BIOMASS_WOOD",
                       "ELECTRICITY",
                       "ELECTRICITY",
                       "ELECTRICITY",
                       "ELECTRICITY",
                       "COMMUNITY_HEAT",
                       "COMMUNITY_HEAT",
                       "BIOMASS_PELLETS",
                       "BIOMASS_PELLETS")) # TODO is this sensible?

  # Top code adults (3) and children (3) and bedrooms (5) and number of habitable rooms (8):
  # This is to avoid an unnecessarily large number of combinations of variables that could 
  # potentially reduce the efficiency and accuracy of the reweighting process
  liw.data$adults <- ifelse(liw.data$adults >= 3, 3, liw.data$adults)
  liw.data$children <- ifelse(liw.data$children >= 3, 3, liw.data$children)
  liw.data$numofbedrooms <- ifelse(liw.data$numofbedrooms >= 5, 5, liw.data$numofbedrooms)
  liw.data$numofhabitalrooms <- ifelse(liw.data$numofhabitalrooms >= 8, 8, liw.data$numofhabitalrooms)
  
  return(liw.data)
}


income.uprating <- function(liw.data) {
  
  
  income.cuts <- wtd.quantile(liw.data$fpfullinc, weights = liw.data$GR2, probs = c(0,0.11111, 0.22222, 0.33333, 0.44444, 0.55555, 0.66666, 0.77777, 0.88888, 1.0), normwt = TRUE, na.rm = FALSE)
  income.decile <- cut(liw.data$fpfullinc, income.cuts, include.lowest = TRUE, labels = c(1:9))
  liw.data$ASHE_dcode <- as.numeric(income.decile)
  
  liw.data$hrpwk <- 0
  liw.data$hrpwk[liw.data$hrpsex == "Male" & liw.data$p5du == "Working full-time (30 hours a week or more)" ] <- 1
  liw.data$hrpwk[liw.data$hrpsex == "Male" & liw.data$p5du == "Working part-time (less than 30 hours a week)" ] <- 2
  liw.data$hrpwk[liw.data$hrpsex == "Female" & liw.data$p5du == "Working full-time (30 hours a week or more)" ] <- 3
  liw.data$hrpwk[liw.data$hrpsex == "Female" & liw.data$p5du == "Working part-time (less than 30 hours a week)" ] <- 4
  
  #lookup income changes
  #ashe.location <- file.path(getwd(),"data/LiW-2008/WalesIncomeChanges.csv")
  ashe.data <- produce.wales.income.uprating.lup()
  
  liw.data <- join(liw.data, ashe.data, by = c("hrpwk", "ASHE_dcode"))
  
  liw.data$householdincomebeforetax <- liw.data$fpfullinc * liw.data$change.2008.to.2014
  
  return(liw.data)
    
}


select.key.variables <- function(liw.data) {
  
  #select variables of interest
  liw.data <- subset(liw.data, select = c("addno",
                                          "GR2",
                                          "builtformtype",
                                          "buildyear",
                                          "morphologytype",
                                          "externalwallconstructiontype",
                                          "cavitywallinsulation",
                                          "insulationthickness",
                                          "numofbedrooms",
                                          "numofhabitalrooms",
                                          "tenuretype",
                                          "adults",
                                          "children",
                                          "householdincomebeforetax",
                                          "chiefincomeearnersage",
                                          "hasdisabledorsickoccupant",
                                          "mainheatingfuel"))

  print("")
  return(liw.data)
  
}


recode.report <- function(column) {
  table(column, useNA = "ifany")
}


