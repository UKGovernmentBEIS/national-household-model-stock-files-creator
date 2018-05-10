#' ---
#' title: "England survey stock conversion"
#' author: "Centre for Sustainable Energy"
#' output: pdf
#' toc: true
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(eval=FALSE)

#'\pagebreak
#'
#'##Libraries
#'
#'Loading the libaries required to run script
#'
#'Note: these libraries must already be installed. 
#' To install libraries use install.packages("nameoflibrary)
#' in R console
#+ warning = FALSE, message = FALSE, comment = NA

#'##Source functions
#'
#'These files can be found in the scotland folder and
#' can be opened in R studio.
#'
#'The assumptions made to form the stock are found in these
#' files within functions. Some minor edits to the files can be
#' made if a different assumption is required.
#Sources (makes available) all functions required to make
#the stock.
source("cases.R",chdir = T)
source("stories.R",chdir = T)
source("elevations.R",chdir = T)
source("occupants.R",chdir = T)
source("roofs.R",chdir = T)
source("lighting.R",chdir = T)
source("spaceheating.R", chdir = T)
source("waterheating.R", chdir = T)
source("ventilation.R", chdir = T)
source("additional-properties.R", chdir = T)
source("DTO-import-files.R", chdir = T)
source("people.R", chdir = T)



#' The main entry point for making english data
#' 
#' @param path.to.ehcs - where the EHS SPSS file is
#' 
#' @param path.to.output - a directory to put the output files in

make.stock <- function(path.to.ehcs, path.to.output) {
  print(paste("Loading", path.to.ehcs))
  
  #' Read in required properties from survey files into a single wide data-frame
  allEntries <- merge.all.sav.files(path.to.ehcs)
  
  #' The following are a special case, where there are more than one enty per aacode 
  #' and adding to all entries table is not necessary.
  doorEntries <- read.spss.with.aacode(file.path(path.to.ehcs, "physical/doors_sl_protect.sav"))
  peopleEntries <- read.spss.with.aacode(file.path(path.to.ehcs, "interview/people_sl_protect.sav"))
  roomsEntries <- read.spss.with.aacode(file.path(path.to.ehcs, "physical/introoms_sl_protect.sav"))
  print("Done all door/room/people stuff")
  
  #' Construct DTO's
  casesDTO <- cases.make(allEntries)
  print("made cases DTO")
  elevationsDTO <- elevations.make(allEntries, doorEntries)
  occupantsDTO <- occupants.make(allEntries, peopleEntries)
  roofsDTO <- roofs.make(allEntries)
  lightingDTO <- lighting.make(roomsEntries)
  spaceHeatingDTO <- space.heating.make(allEntries, path.to.ehcs, path.to.output)
  waterHeatingDTO <- make.waterheating(allEntries, spaceHeatingDTO, path.to.ehcs)
  ventilationDTO <- make.ventilation(allEntries, elevationsDTO)
  additionalpropertiesDTO <- make.additionalproperties(allEntries)
  IImportLogDTO <- make.eng_IImportLogDTO(allEntries)
  IStockImportMetadataDTO <- make.eng_IStockImportMetadataDTO(allEntries)
  metadata <- make.eng_metadata(allEntries)
  peopleDTO <- people.make(peopleEntries)

  print("All DTO data-frame constructed, saving to CSV files...")
  
  #' Output DTO's
  save.dto(casesDTO, file.path(path.to.output, "cases.csv"))
  print("made cases.csv")
  save.dto(elevationsDTO, file.path(path.to.output, "elevations.csv"))
  save.dto(occupantsDTO, file.path(path.to.output, "occupants.csv"))
  save.dto(roofsDTO, file.path(path.to.output, "roofs.csv"))
  save.dto(lightingDTO, file.path(path.to.output, "lighting.csv"))
  save.dto(spaceHeatingDTO, file.path(path.to.output, "space-heating.csv"))
  save.dto(waterHeatingDTO, file.path(path.to.output, "water-heating.csv"))
  save.dto(ventilationDTO, file.path(path.to.output, "ventilation.csv"))
  save.dto(peopleDTO, file.path(path.to.output, "people.csv"))

  print("Core DTO's saved, saving additional properties and logs to csv...")
  
  save.dto(additionalpropertiesDTO, file.path(path.to.output, 
                                              "additional-properties.csv"))
  save.eng_IImportLogDTO(IImportLogDTO, file.path(path.to.output, "IImportLogDTO.csv"))
  save.eng_IStockImportMetadataDTO(IStockImportMetadataDTO, 
                            file.path(path.to.output, "IStockImportMetadataDTO.csv"))
  save.eng_metadata(metadata, file.path(path.to.output, "metadata.csv"))

  print("Addtional properties and logs csv created, making house storeys...")
  
  #' Just do stories on their own as they have a separate bit of code.
  scale.storeys <- if (exists("option.ehs.storeys.scale")) option.ehs.storeys.scale else FALSE

  generate.all.storeys(path.to.ehcs, file.path(path.to.output, "storeys.csv"), scale.storeys)

  print("House storeys created and saved to csv.")
}

merge.all.sav.files <- function(path.to.ehcs){
  #' We use general.sav as our base-line of all cases available 
  allEntries <- read.spss.with.aacode(file.path(path.to.ehcs, "derived/general_13plus14_sl_protect.sav"))
  
  #' Now merge all other spss files that should have just one entry for each house case
  toMerge <- Reduce(function(a, b){
    join(a,b, by = "aacode")
  },Map(function(name){
       read.spss.with.aacode(file.path(path.to.ehcs, name))
  }, c("physical/firstimp_ps_sl_protect.sav",
       "physical/shape_sl_protect.sav",
       "physical/interior_sl_protect.sav",
       "derived/physical_13plus14_sl_protect.sav",
       "physical/around_sl_protect.sav",
       "physical/services_sl_protect.sav",
       "physical/flatdets_sl_protect.sav", 
       "derived/interview_13plus14_sl_protect.sav",
       "interview/rooms_sl_protect.sav", 
       "physical/elevate_sl_protect.sav", 
       #"fuel_poverty/fuel_poverty_dataset_2014_tc.sav",
       #"fuel_poverty/fuel_poverty_dataset_2014_supplementary_variables_tc_sl_protect.sav",
       "fuel_poverty/dataforukda_final_disclosure_control_revised_sl_protect.sav")))
  
  merged <- join(allEntries, 
                 toMerge,
                 by = "aacode")

  print("First pass merge of sav files into wide data-frame completed, adding dimensions_13plus14_sl_protect.sav...")
  #' Dimensions sav file uses different case for Aacode column so we need to-do a 
  #' different merge   
  allEntries <- merge(merged, 
                     read.spss.with.aacode.ucase(file.path(path.to.ehcs, "derived/detailed/dimensions_13plus14_sl_protect.sav")),
                     all.x = TRUE,
                     by.x = "aacode", by.y = "Aacode")

  print("Dimensions sav file merged adding rooms summary from introoms.sav...")
   
  # Create room summary and merge with allEntries data.frame
  introoms <- read.spss.with.aacode(file.path(path.to.ehcs, "physical/introoms_sl_protect.sav"))
  case.room.summary <- summarise.rooms(introoms)
  allEntries <- merge(allEntries, case.room.summary, all.x = TRUE, by = "aacode")

  print("all entries created")
  
  write.csv(allEntries, file.path(getwd(), "allentries2014.csv"), row.names=FALSE, na ="NULL")
  print("all entries PRINTED")
  
  return(allEntries)
}

#' Constructs summary information from all room files for a specific house case
#'
#'
summarise.rooms <- function(introoms){
  summary <- cast(introoms, aacode  ~ type, value = "Finflrsf")
  names(summary) <- c("aacode", 
                      "livingRoomHasSolidFloor", 
                      "kitchenHasSolidFloor", 
                      "bedroomHasSolidFloor", 
                      "bathroomHasSolidFloor", 
                      "circulationHasSolidFloor")

   return(summary)
}

save.dto <- function(dto, output){
  write.csv(dto, file=output, row.names=FALSE, na="")
}

#+ child=c('spaceheating.R','roofs.R','cases.R','lighting.R','occupants.R', 'ventilation.R','waterheating.R','stories.R','elevations.R','additional-properties.R')

#'
#' \pagebreak
#'
#'#Cases
#'
#'The following section of code is generated in [cases.R](cases.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("cases.R")


#'
#' \pagebreak
#'
#'#Roofs
#'
#'The following section of code is generated in [roofs.R](roofs.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("roofs.R")

#'
#' \pagebreak
#'
#'#Lighting
#'
#'The following section of code is generated in [lighting.R](lighting.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("lighting.R")

#'
#' \pagebreak
#'
#'#Occupants
#'
#'The following section of code is generated in [occupants.R](occupants.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("occupants.R")

#'
#' \pagebreak
#'
#'#Spaceheating
#'
#'The following section of code is generated in [spaceheating.R](spaceheating.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("spaceheating.R")

#'
#' \pagebreak
#'
#'#Waterheating
#'
#'The following section of code is generated in [waterheating.R](waterheating.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("waterheating.R")

#'
#' \pagebreak
#'
#'#Storeys
#'
#'The following section of code is generated in [stories.R](stories.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("stories.R")

#'
#' \pagebreak
#'
#'#Ventilation
#'
#'The following section of code is generated in [ventilation.R](ventilation.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("ventilation.R")

#'
#' \pagebreak
#'
#'#Elevations
#'
#'The following section of code is generated in [elevations.R](elevations.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("elevations.R")

#'
#' \pagebreak
#'
#'#Additional properties
#'
#'The following section of code is generated in [additional-properties.R](additional-properties.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("additional-properties.R")
