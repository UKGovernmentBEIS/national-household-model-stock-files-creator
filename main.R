#' ---
#' title: "R Stock Conversion"
#' author: "Centre for Sustainable Energy"
#' date: "2015" 2017 update
#' 

library(Hmisc)
library(memisc)
library(foreign)
library(knitr)
library(RUnit)
library(plyr)
library(reshape)
library(tidyr)
library(data.table) #Faster and more feature version of data.frame
library(stringr)
library(dplyr) #Required for dataframe manipulation
library(ggplot2) #Required to create plots
library(grid) #Required to create plots
library(splancs) # required for areapl function

library(utils) # for zip
#' ## Controllable settings
#' EHS space heating lookup tables to use, in preference order
#' You can comment these out if you don't want them.
option.ehs.spaceheating.lookups <- c(
    #from SAP:
    "lup-boiler-4b.csv",
    "electric-boiler-lup.csv",
    "gas-room-heater-lup.csv",
    "oil-room-heater-lup.csv",
    "other-room-heater-lup.csv",
    "solid-fuel-boiler-lups.csv",
    "storage-heater-lup.csv",
    "warm-air-lup.csv",
    # from the CAR methodology
    "Finchbcd-lup.csv",
    # final fallback - best to leave this!
    "space-heating-missing-data-lup.csv"
)

#' Whether to build the scotland stock - if false UK stock is not built
option.scotland.build <- T

#' Whether to build the english stock - if false UK stock is not built
option.england.build <- T

#' Whether to build the welsh stock - if false UK stock is not built
option.wales.build <- T

#' Whether to document the stock creation process - if false documentation is not built
option.documentation.build <- F

#' Whether to document the stock creation process - if false documentation is not built
option.documentation.build <- FALSE

#' Whether to use the sedbuk lookup device
option.ehs.spaceheating.sedbuk <- TRUE

#' Whether to run the boiler matching jar file as part of that
option.ehs.spaceheating.sedbuk.run <- TRUE

#' Whether to generate summaries
#' requires the NHM jar!
option.generate.summaries <- FALSE

#' Whether to scale the total floor area of the storeys produced for a house
#' to match the EHS variable FloorArea from derived/dimensions.sav
option.ehs.storeys.scale <- T

#' A string to prefix the name of stock files with
#' Add a test in here so if we set scaled to TRUE it calls the stock Scaled, OTHERWISE calls it Unscaled
option.stock.name.prefix <- "scaled-"

imputation.schema.path <- file.path(getwd(), "data", "imputation-schema.xlsx")

summary.main.path <- file.path(getwd(), "summary", "main.Rmd")

summarise.stock.file <- function(stock.name) {
  stock.path <- getwd()
  rmarkdown::render(summary.main.path, "pdf_document", envir = new.env(),
                    output_dir = getwd(),
                    output_file = paste(stock.name, "-summary.pdf", sep=""))
}

make.zip.file <- function(path, name) {
  name <- paste(option.stock.name.prefix, name, sep="")
  ## copy the imputation schema into place
  file.copy(imputation.schema.path, path)
  ## make the zip file
  wd <- getwd()
  setwd(path)
  zname <- paste(name, ".zip", sep="")
  if (file.exists(zname)) unlink(zname)
  
  zip(zname,
      files=list.files(".", pattern="(\\.csv|\\.xlsx)$"))
  if (option.generate.summaries) summarise.stock.file(name)
  setwd(wd)
}

erase.dto.files <- function(path) unlink(list.files(path, full.names=T, pattern="(\\.csv|\\.xlsx)$"))

#' main entry point
# setwd("~/software-projects/r-nhm-stock-creator/")
outputs <- file.path(getwd(), "outputs")

## S C O T L A N D ## 

#' Create Scotland stock files
#' 
#' run the scotland code in a separate env (so we don't replace any important stuff)
#+ warning = FALSE, message = FALSE, comment = NA
scotland.outputs <- file.path(outputs, "scotland")
print(paste("Skipping Scotland=", !option.scotland.build))
if(option.scotland.build){
  erase.dto.files(scotland.outputs)
  
  # Create output folder if does not exist
  if (file_test("-d",scotland.outputs) == FALSE){
    dir.create(scotland.outputs, recursive=T)
  }
  scotland <- new.env()
  scotland.survey <- file.path(getwd(), "data/SHCS_11-13/external_cse_data.sav")
  sys.source("scotland/main.R", envir=scotland, chdir=T)
  with(scotland,
       make.scotland(scotland.survey, scotland.outputs))
  
  make.zip.file(scotland.outputs, "scotland-shcs-11-13")
  
  if(option.documentation.build){
    scotland.tests <- new.env()
    rmarkdown::render("scotland/scotland-test-results.R", "pdf_document", envir = scotland.tests
                      ,output_dir = file.path(dirname(getwd()),"Reports"))
    
    scotland.stockreport <- new.env()
    rmarkdown::render("scotland/main.R", "pdf_document", envir = scotland.stockreport
                      ,output_file = "Scotland-stock-creation-code.pdf")
  }
}

##  E N G L A N D ##

#' Create English Stock
england.outputs <- file.path(outputs, "england")
print(paste("Skipping England=", !option.england.build))
if(option.england.build){
  erase.dto.files(england.outputs)
  # Create output folder if does not exist
  if (file_test("-d",england.outputs) == FALSE){
    dir.create(england.outputs, recursive=T)
  }
  england <- new.env()
  path.to.ehcs <- file.path(getwd(), "data/EHS_2014")
  sys.source("ehcs/main.R", envir=england, chdir=T)
  with(england, 
       make.stock(path.to.ehcs, england.outputs))
  
  make.zip.file(england.outputs, "england-ehcs-2014")
  
  if(option.documentation.build){
    england.tests <- new.env()
    rmarkdown::render("ehcs/england-test-results.R", "pdf_document", envir = england.tests
                      ,output_dir = file.path(dirname(getwd()),"Reports"))
    
    england.stockreport <- new.env()
    rmarkdown::render("ehcs/main.R", "pdf_document", envir = england.stockreport
                      ,output_file = "England_stock_creation_code.pdf")
  }
}

##  W A L E S ##
wales.outputs <- file.path(outputs, "wales")
if(option.wales.build){
  erase.dto.files(wales.outputs)
  
  # Create output folder if does not exist
  if (file_test("-d",wales.outputs) == FALSE){
    dir.create(wales.outputs, recursive=T)
  }
  
  wales <- new.env()
  path.to.wales <- file.path(getwd(), "data/LiW-2008")
  sys.source("wales/main.R", envir = wales, chdir = T)
  with(wales, 
       make.wales(path.to.wales, wales.outputs))
  make.zip.file(wales.outputs, "wales-ehcs-2014")
  print("Wales stock zip created")
}

## COMBINED ALL STOCK AND CREATE A UK STOCK (WAITING FOR NORTHERN IRELAND) ##
can.make.uk <- option.scotland.build & option.england.build & option.wales.build
print(paste("Skipping Full UK Stock=", !can.make.uk))
if(can.make.uk){
  #+ warning = FALSE, message = FALSE, comment = NA
  uk.outputs <- file.path(outputs, "uk")
  erase.dto.files(uk.outputs)
  
  
  # Create output folder if does not exist
  if (file_test("-d",uk.outputs) == FALSE){
    dir.create(uk.outputs, recursive=T)
  }
  
  uk <- new.env()
  sys.source("combinestock.R", envir=uk, chdir=T)
  
  with(uk,
       make.uk(scotland.outputs,england.outputs,wales.outputs,uk.outputs))
  
  make.zip.file(uk.outputs, "GB-stock")
}
