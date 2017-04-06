#' ---
#' title: "Occupants functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make occupants data
#'
#'Create a .csv file using the function make.occupants, which creates a dataframe 
#'containing a complete set of populated variables for the occupants.csv stock 
#'file.
#'
#'The information on occupants covers income, chief income earner's age, chief income
#'earner's working hours, details on whether anyone in the dwelling is long term sick
#'or disabled, whether anyone in the household is one benefits and how long the 
#'current inhabitants have lived in the dwelling. Not all this information is 
#'required and some variables can be left NULL/blank.
#'
#' @param allEntries
#' 
#' @param peopleEntries 

occupants.make <- function(allEntries, peopleEntries){
  surveyYear <- 2014
  dateMovedIn <- ifelse(is.na(allEntries$lenres), NA, surveyYear - allEntries$lenres)
  dateMovedIn <- ifelse(is.na(dateMovedIn), "NULL", 
                        as.POSIXct(paste(c(dateMovedIn),c("01"),c("01"),sep = "-"), 
                                   format = "%Y-%m-%d", tz = "UTC"))  
  
  allOccupants <- data.frame(
    aacode = allEntries$aacode,
    chiefincomeearnersage = ifelse(is.na(allEntries$agehrpx), "", allEntries$agehrpx),
    householdincomebeforetax = allEntries$fpfullinc,
    hasoccupantonbenefits = ifelse(is.na(allEntries$hhvulx), "", 
                                   ifelse(allEntries$hhvulx == "yes", TRUE, FALSE)),
    hasdisabledorsickoccupant = ifelse(is.na(allEntries$hhltsick), "", 
                                       ifelse(allEntries$hhltsick == "yes", TRUE, FALSE)),
    datemovedin = dateMovedIn
  )
  
  #Add Working Hours
  hrpPersons <- subset(peopleEntries, peopleEntries$PERSNO == peopleEntries$HRP)
  allHours <- data.frame(
    aacode = hrpPersons$aacode,
    workinghours = hrpPersons$NoOfHrsR  
  )
 
  allOccupants <- join(x = allOccupants, y = allHours, by = c("aacode"), type = c("left"))
  
  print(paste("occupants DTO complete; number of records: ", nrow(allOccupants)))
  
  return (allOccupants)
}
