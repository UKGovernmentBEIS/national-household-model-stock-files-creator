#' ---
#' title: "Roofs functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save roofs data
#'
#'Create a .csv file using the function make.roofs, which creates a dataframe 
#'containing a complete set of populated variables for the roofs.csv stock 
#'file.
#'
#'The roofs stock file contains four variables with information on the roof structure
#', the roof covering material, roof construction type and the level of insulation in
#' the roof/loft (depending on type of roof is present).
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
save.roofs <- function(shcs, output) {
  write.csv(make.roofs(shcs), file=output, row.names=FALSE)
}

#'Make the dataframe that contains all the information required for the roofs.csv
#' file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.roofs <- function(shcs) {
  the.coveringtype <- covering.type(shcs$Q21)
  the.structuretype <- structure.type(shcs$Q19,
                                      building.type(shcs$C1, shcs$C2))
  the.constructiontype <- construction.type(the.structuretype,
                                            the.coveringtype)
  the.insulationthickness <- insulation.thickness(shcs$loftins, shcs$M26)
  
  data.frame(aacode = shcs$uprn_new
             ,coveringtype = the.coveringtype
             ,insulationthickness = the.insulationthickness
             ,contructiontype = the.constructiontype
             ,structuretype = the.structuretype
  )
}

#'
#' \pagebreak
#'
#'##Roof type
#'
#'Map covering type
#'
#' This is entirely produced from Q21 in the SHCS.
#' - 'asbestos' has no cognate in the NHM, and so is taken to be 'asphalt'
#' - missing data is taken to be 'tile'
#' 
#' @param Q21 - column Q21 (principle roof cover) from the scottish survey 
covering.type <- function(Q21) {
    ## preconditions
    as.factor(checked.revalue(
        Q21,
        c("Slates" = "slates",
          "Tiles" = "tile",
          "Felt" = "felt",
          "Asphalt" = "asphalt",
          "Asbestos" = "asphalt",
          "Metal" = "metal",
          "Other" = "thatch",
          "Not applicable" = "tile",
          "Unobtainable" = "tile")))
}

#'Roof structure type
#' 
#'The principal roof type column is used to map against roof types for the NHM,
#' the building type is used in a set of rules to fill in missing values
#' 
#'@param Q19 - column Q19 (Principal roof type) from the scottish survey
#' 
#'@param buildingtype - a combined building type (see common.R, building_type)
structure.type <- function(Q19, buildingtype) {
  #If Q19 is unobtainable, a default is chosen based on the building type:
      # flats are assigned a flat roof whilst all other dwellings a pitched roof
  Q19.with.defaults <- as.factor(ifelse(Q19 == "Unobtainable",
                                        ifelse(is.flat(buildingtype),
                                               "Flat",
                                               "Pitched"),
                                        as.character(Q19)))
  #Having filled in the unobtainable values, then recode to the NHM convention.
  #Note: mandard is a standardised spelling mistake in the NHM
  checked.revalue(Q19.with.defaults,
                  c("Pitched" = "pitched",
                    "Flat" = "flat",
                    "Mono" = "flat",
                    "Mansard" = "mandard", #sic
                    "Half mansard" = "mandard"))
}

#'Infer roof construction type
#' 
#'The roof constuction type is inferred from the structure type and the covering
#' type as described in the function below.
#' 
#'@param structuretype - per the structure.type function above
#' 
#'@param coveringtype - per the covering.type function above.
construction.type <- function(structuretype, coveringtype) {
  ifelse(
    structuretype == "pitched" | structuretype == "mandard",
    "pitchedslateortiles",
    ifelse(coveringtype == "thatch",
           "thatched",
           "flat"))
}


#'
#' \pagebreak
#' 
#'##Roof insulation
#'
#'Map insulation thickness
#'
#'This is produced using 'loftins', except where that is invalid, in which case a
#' value is taken from 'M26.' 'loftins' is a derived variable and M26 is the raw 
#' survey information. 'M26' values are only used when 'loftins' is missing data.
#' The following logic is applied to values in the stock data:
#' - none, flat roof unmeasured, not applicable, and unobtainable are all taken to
#'   be 0
#' - 300mm and above is taken to be 300mm.
#'
#'@param loftins - column loftins (Roof/loft insulation + imputed) from the
#'  scottish survey
#' 
#'@param M26 - column M26 from the scottish survey
insulation.thickness <- function(loftins, M26) {
  #Recode the loftins column
  from.loftins <- checked.revalue(
    loftins,
    c("none" = "0",
      "flat roof unmeasured" = NA,
      "Not Applicable" = NA,
      "Unobtainable" = NA,
      "12mm"  = "0",  "25mm"  = "25",
      "50mm"  = "50",  "75mm"  = "75",
      "100mm" = "100", "150mm" = "150",
      "200mm" = "200", "250mm" = "250",
      "300mm and over" = "300"))
  
  #Overwrite any NA values in the result with M26 values
  #M26 has a slightly different coding to loftins.
  from.m26 <- 
    checked.revalue(
      M26,
      c("None" = "0",
        "12mm"  = "0",  "25mm"  = "25",
        "50mm"  = "50",  "75mm"  = "75",
        "100mm" = "100", "150mm" = "150",
        "200m" = "200",  "250mm" = "250",
        "300mm or more" = "300",
        "Flat roof" = NA,
        "Not applicable" = NA,
        "Unobtainable" = NA))
  
  #The two columns are combined using the loftins, unless it is an NA in which
  #case M26 is used.
  #Both columns are converted to characters first to make it easy to combine
  from.both <- ifelse(is.na(from.loftins),
                      as.character(from.m26),
                      as.character(from.loftins))
  
  #Any remaining NAs are turned into zeros
  from.both[is.na(from.both)] <- "0"
  
  #The results from combining both columns are then turned into a numeric
  as.numeric(from.both)
}
