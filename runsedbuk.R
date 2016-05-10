#' ---
#' title: "Run Sedbuk"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#' Run code to create the sedbuck input csv file that is used by the java boiler 
#' matching programme for Scottish stock.
#' 
#' The sedbuck input.csv file is created within the output folder of Scotland

#'Loading the libaries required to run script
#'
#'Note: these libraries must already be installed. 
#' To install libraries use install.packages("nameoflibrary)
#' in R console
#+ warning = FALSE, message = FALSE, comment = NA
library(foreign) #Required to read spss files


outputs <- file.path(getwd(), "outputs")

#+ warning = FALSE, message = FALSE, comment = NA
scotland.outputs <- file.path(outputs, "scotland")
# Create output folder if does not exist
if (file_test("-d",scotland.outputs) == FALSE){
  dir.create(scotland.outputs, recursive=T)
}

#'Imports scottish survey data and then calls make.sedbuk to create the output file
#+ warning = FALSE, message = FALSE, comment = NA
sedbuk <- new.env()
scotland.survey <- file.path(getwd(), "data","SHCS_11-13","external_cse_data.sav")
sys.source("scotland/sedbuk.R", envir=sedbuk, chdir=T)
with(sedbuk,
     make.sedbuk(scotland.survey, scotland.outputs))
