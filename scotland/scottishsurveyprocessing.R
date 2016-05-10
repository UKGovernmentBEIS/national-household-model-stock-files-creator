#' ---
#' title: "Scottish survey preprocessing"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'The inconsistent values in the survey are preprocessed
#'
#'The values that are fixed are those that have an impact on stock creation,
#' the survey data has not been checked for complete consistancy, only those
#' variables that have been used;
#' i.e. an open fire fueled by electricity is not a physical
#' possibility and so assumptions have been made for this
#' case about what is likely to exist in order to create spaceheating
#' 
#' @param shcs - the scottish survey data
clean.survey <- function(shcs){
  #The scottish survey data has 22 cases where the only data that exists is
  #the uprn_new (aacode). These cases are removed prior to processing.This
  #should not be altered.
  shcs <- subset(shcs,is.na(shcs$year)=="FALSE")
  
  #These cases have a second extension with area and external perimeter
  #variables, but a missing ceiling height.These have been filled with the
  #standard height of 2.4m
  shcs$N7_C[shcs$uprn_new == 4950999308] <- 2.4
  shcs$N7_C[shcs$uprn_new == 3485528102] <- 2.4
  shcs$N7_C[shcs$uprn_new == 9153471654] <- 2.4
  
  #This case is a flat comprised entirely of a room in roof, with two extensions
  #in order to correctly calculate the position of the extensions this variables
  #stored in the room in roof variables (N5_A and N5_C) are moved to those for level
  #one. The external perimiter is calculated using the area and assuming the ratio
  #length/width is 1.5, this is the only figure that should be changed.
  dimension.ratio <- 1.5
  shcs$N1_A[shcs$uprn_new == 8859831108] <- shcs$N5_A[shcs$uprn_new == 8859831108] 
  shcs$N1_C[shcs$uprn_new == 8859831108] <- shcs$N5_C[shcs$uprn_new == 8859831108] 
  shcs$N1_D[shcs$uprn_new == 8859831108] <- 
    (sqrt(shcs$N5_A[shcs$uprn_new == 8859831108]*dimension.ratio)+
       ((sqrt(shcs$N5_A[shcs$uprn_new == 8859831108]/dimension.ratio))*2))
  
  #This case is missing level one external perimiter, this is replaced with a value
  #(33) that was calculated from the area, using the assumption that length/width is
  #1.5
  shcs$N1_D[shcs$uprn_new == 460193204] <- 33
  
  #This case is missing a value for the extent of primary wall. This has been set
  #to 10 as it is the most common extent in the survey data
  shcs$Q10[shcs$uprn_new == 4196979699] <- 10 
  
  #The principal hot water heating source value for the following cases were
  #inconsistent with the other values of heating, they are changed here to values
  #that are both consistant and most common.
  shcs$M17[shcs$uprn_new==6379341481] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==1060005515] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==9472432770] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==7353370291] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==7950759180] <- "Elec immersion"
  shcs$M17[shcs$uprn_new==2676458351] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==2177514151] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==1676502308] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==8873452423] <- "Room heater BB"
  shcs$M17[shcs$uprn_new==5700173539] <- "Room heater BB"
  
  #The water heating fuel value for the following case was set as unobtainable.
  #It has been changed to off-peak electric to match that of the spaceheating fuel.
  shcs$M18[shcs$uprn_new==7403106567] <- "Off-peak electric"
  
  
  #The primary heating fuel in these two cases are changed to oil as the primary
  #heating fuel in the survey is "other" and the primary form of heating is a boiler
  shcs$M5[shcs$uprn_new == 6115831852] <- "Oil"
  shcs$M5[shcs$uprn_new == 4837178045] <- "Oil"
  
  #The primary heating fuel in these two cases are changed to Gas (mains) as the
  #primary heating fuel was communal heating,no CHP with a warm air system
  shcs$M5[shcs$uprn_new == 1585265934] <- "Gas (mains)"
  
  #The year of heating system is set to +1998 as the survey records it as not
  #applicable and the flue type is set to balanced as it is also recorded as not
  #applicable
  shcs$M6[shcs$uprn_new == 6265548876] <- "1998+"
  shcs$M8[shcs$uprn_new == 6265548876] <- "Balanced"
  
  #The flue type is changed from balanced to fan assisted as the combination of
  #heating system (condensing combi, gas boiler, pre 1998) with a balanced flue is
  #not an option in SAP heating tables.
  shcs$M8[shcs$uprn_new == 122595215] <- "Fan assisted"
  shcs$M8[shcs$uprn_new == 2065580083] <- "Fan assisted"
  shcs$M8[shcs$uprn_new == 8846926949] <- "Fan assisted"
  shcs$M8[shcs$uprn_new == 2071095953] <- "Fan assisted"
  shcs$M8[shcs$uprn_new == 7052823383] <- "Fan assisted"
  shcs$M8[shcs$uprn_new == 155273248] <- "Fan assisted"
  
  #The flue type is changed from balanced to fan assisted as the combination of
  #heating system (condensing standard, gas boiler, pre 1998) with a balanced flue is
  #not an option in SAP heating tables.
  shcs$M8[shcs$uprn_new == 3127122104] <- "Fan assisted"
  shcs$M8[shcs$uprn_new == 8853207500] <- "Fan assisted"
  
  #Value in solid fuel heater is set to not applicable (changed from closed fire)
  #as the heating system is defined as a room heater using peak electric
  shcs$M15[shcs$uprn_new == 2314543027] <- "Not applicable"
  
  #Value in solid fuel heater is set to closed fire (changed from not applicable)
  #as the heating system is run on house coal in heated space
  shcs$M15[shcs$uprn_new == 7212136000] <- "Closed fire"
  shcs$M15[shcs$uprn_new == 1600903823] <- "Closed fire"
  
  #Value in solid fuel heater is set to closed fire (changed from not applicable)
  #as the heating system is run on Antracite nuts and grain in heated space
  shcs$M15[shcs$uprn_new == 8311930088] <- "Closed fire"
  
  #The percentage of double glazing column (Q47) contains 17 cases where the tenths
  #have been set to 55, this is not a valid option (10 is the highest value allowed)
  #due to the position of 5 and 8 on the numpad it has been assumed that these cases
  #should have been 88 (not applicable). Both of these values have been set to 0 for
  #calculations during elevations. This number can be set to any integer between 0
  #and 10
  shcs$Q47[shcs$Q47 == 55] <- 0 
  shcs$Q47[shcs$Q47 == 88] <- 0 
  
  #There are 22 cases which have no habitable floors (excluding room in roof)
  #for stock creation they have been given one floor - this value should not be
  #changed as it ensures the polygons for storeys are created correctly.
  shcs$J2[shcs$J2 == 0] <- 1
  
  #16 cases have missing entries (i.e. no levels) for floor ground floor type. These
  #should be set as 'unobtainable' and will then be allocated a ground floor type 
  #according to the grndfloor.type function defined in cases
  shcs$N1_E[is.na(shcs$N1_E) == TRUE] <- "Unobtainable"
  return(shcs)
}
