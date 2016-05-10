#' ---
#' title: "Scotland survey stock conversion"
#' author: "Centre for Sustainable Energy"
#' output: pdf
#' toc: true
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(eval=FALSE)

#'
#'\pagebreak
#'
#' #Scotland stock conversion function
#' 
#'The following section of code is generated in [scotland/main.R](main.R)
#'
#'This script calls all the functions that create each .csv file.
#' The make.scotland function is run from the main.R script which runs both Scotland
#' and England.
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
source("scottishsurveyprocessing.R",chdir = TRUE)
source("spaceheating.R",chdir = TRUE)
source("roofs.R",chdir = TRUE)
source("cases.R",chdir = TRUE)
source("lighting.R",chdir = TRUE)
source("occupants.R",chdir = TRUE)
source("ventilation.R",chdir = TRUE)
source("waterheating.R",chdir = TRUE)
source("storeys.R",chdir = TRUE)
source("elevations.R",chdir = TRUE)
source("additional-properties.R",chdir = TRUE)
source("DTO-import-files.R",chdir = T)
#functions required to test the stock
#source("test-results.R", chdir=TRUE)

#'
#' \pagebreak
#' 
#' ##Make Scottish stock
#'  
#' The main function which makes the scottish stock, this is run from main.R
#' 
#' The data is first loaded from the survey and cleaned using the
#'  function clean.survey.
#' 
#' This data is then passed to each of the functions (i.e. save.dtofilename)
#'  in turn. Inside these functions the survey data is processed and
#'  turned into a dataframe of the correct format which is then written to
#'  a .csv file stored in the outputs/scotland folder.
#' 
#'Finally the survey data is used by the test function to compare
#' the stock data that has been created by the stock creator.
#' 
#' @param path.to.shcs - file path to SHCS SPSS file
#' 
#' @param path.to.output - directory where output files are put
make.scotland <- function(path.to.shcs, path.to.output) {
  #Load scottish survey data and preprocess some cases which have inconsistent
  #values i.e. an open fire fueled by electricity
  print(paste("Loading", path.to.shcs))
  shcs <- read.spss(path.to.shcs, to.data.frame = TRUE, reencode='utf-8')
  shcs <- clean.survey(shcs)
  
  #Creates the spaceheating file
  spaceheating.path <- file.path(path.to.output, "space-heating.csv")
  print(paste("Creating", spaceheating.path))
  save.spaceheating(shcs, spaceheating.path, dirname(scotland.survey))
  
  #Creates the roofs file
  roofs.path <- file.path(path.to.output, "roofs.csv")
  print(paste("Creating", roofs.path))
  save.roofs(shcs, roofs.path)
  
  #Creates the cases file
  cases.path <- file.path(path.to.output, "cases.csv")
  print(paste("Creating", cases.path))
  save.cases(shcs, cases.path)
  
  #Creates the lighting file
  lighting.path <- file.path(path.to.output, "lighting.csv")
  print(paste("Creating", lighting.path))
  save.lighting(shcs, lighting.path)
  
  #Creates the occupants file
  occupants.path <- file.path(path.to.output, "occupants.csv")
  print(paste("Creating", occupants.path))
  save.occupants(shcs, occupants.path)
  
  #Creates the ventilation file
  ventilation.path <- file.path(path.to.output, "ventilation.csv")
  print(paste("Creating", ventilation.path))
  save.ventilation(shcs, ventilation.path)
  
  #Creates the waterheating file
  waterheating.path <- file.path(path.to.output, "water-heating.csv")
  print(paste("Creating", waterheating.path))
  save.waterheating(shcs,waterheating.path, dirname(scotland.survey),path.to.output)
  
  #Creates the storeys file
  storeys.path <- file.path(path.to.output, "storeys.csv")
  print(paste("Creating", storeys.path))
  save.storeys(shcs, storeys.path)
  
  #Creates the elevations file
  elevations.path <- file.path(path.to.output, "elevations.csv")
  print(paste("Creating", elevations.path))
  save.elevations(shcs, elevations.path)
  
  #Creates the additional properties file
  additionalproperties.path <- file.path(path.to.output, "additional-properties.csv")
  print(paste("Creating", additionalproperties.path))
  save.additionalproperties(shcs, additionalproperties.path)
  
  #create additional csv files for stock import
  IStockImportMetadataDTO.path <- file.path(path.to.output,
                                            "IStockImportMetadataDTO.csv")
  print(paste("Creating", IStockImportMetadataDTO.path))
  save.IStockImportMetadataDTO(shcs, IStockImportMetadataDTO.path)
  
  IImportLogDTO.path <- file.path(path.to.output, "IImportLogDTO.csv")
  print(paste("Creating", IImportLogDTO.path))
  save.IImportLogDTO(shcs, IImportLogDTO.path)    
  
  metadata.path <- file.path(path.to.output, "metadata.csv")
  print(paste("Creating", metadata.path))
  save.metadata(shcs, metadata.path)    
  
  # run end-to-end tests
  #test.allshcsdtotests(shcs, path.to.output)
}

#+ child=c('scottishsurveyprocessing.R','spaceheating.R','roofs.R','cases.R','lighting.R','occupants.R','ventilation.R','waterheating.R','storeys.R','elevations.R','additional-properties.R','DTO-import-files.R')

#'
#' \pagebreak
#'
#'#Preprocessing Scottish Survey Data
#'
#'The following section of code is generated in [scottishsurveyprocessing.R](scottishsurveyprocessing.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("scottishsurveyprocessing.R")

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
#'The following section of code is generated in [storeys.R](storeys.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("storeys.R")

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

#'
#' \pagebreak
#'
#'#Additional DTO import files
#'
#'The following section of code is generated in [DTO-import-files.R](DTO-import-files.R)
#'
#+ eval=TRUE, echo = FALSE
spin_child("DTO-import-files.R")
