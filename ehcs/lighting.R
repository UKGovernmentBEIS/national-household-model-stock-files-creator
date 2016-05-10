#' ---
#' title: "Lighting functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'#Make lighting data
#'
#'Create a .csv file using the function make.lighting, which creates a dataframe 
#'containing a complete set of populated variables for the lighting.csv stock file.
#'
#'The lighting dataframe contains only one variable - the fraction of lighting in the 
#' dwelling which is low energy lighting.
#'
#' @param introoms - the the introoms sav file data frame
#' 
lighting.make <- function(introoms){
  
  roomsWithWeights <- data.table(
    aacode = introoms$aacode,
    type = introoms$type,
    hasLowEnergyLights = ifelse(is.na(introoms$Finhtglg), FALSE, 
                                ifelse(introoms$Finhtglg == "Yes", TRUE, FALSE)),
    weight = room.weighting(introoms$type)
  )
  
  # Create sum weights for each room in house case
  dominator <- roomsWithWeights[, j=(sum(weight)), by = aacode]
  setnames(dominator, c("aacode", "dominator"))
  
  # Create sum weight for each room in a house case that has low energy lights
  roomsWithWeights <- subset(roomsWithWeights, hasLowEnergyLights == TRUE)
  numerator <- roomsWithWeights[, j=(sum(weight)), by = aacode]
  setnames(numerator, c("aacode", "numerator"))
  
  # Merge the weights
  merged <- join(dominator, numerator, by = "aacode")
  
  # Compile the results
  lights <- data.frame(
    aacode = merged$aacode,
    fraction = ifelse(is.na(merged$numerator/merged$dominator),0,
                      merged$numerator/merged$dominator)
  ) 
  
  print(paste("lighting DTO complete; number of records: ", nrow(lights)))
  
  return(lights)
}

#' ##Determine number of fittings in each room
#' 
#' Apply weighting of rooms in order to determine the number of lightbulbs found in 
#' each room.
#' 
room.weighting <- function(TYPE){
  return (as.numeric(levels(room.weighting.lookup(TYPE))[room.weighting.lookup(TYPE)]))
}

#' Gets the weighting for a room to be used in the Low Energy Lighting
#' calculation, as specified in Bredem 8 section 4.2.
#'
#' @param TYPE - Room Type
room.weighting.lookup <- function(TYPE){
  as.factor(checked.revalue(
    TYPE,c(
      "Living room" = 2,
      "Kitchen" = 2,
      "Bedroom" =  1,
      "Bathroom" = 1,
      "Circulation" = 2
      )))}
