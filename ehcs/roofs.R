#' ---
#' title: "Roofs functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make roofs data
#'
#'Create a .csv file using the function make.roofs, which creates a dataframe 
#'containing a complete set of populated variables for the roofs.csv stock 
#'file.
#'
#'The roofs stock file contains four variables with information on the roof structure
#', the roof covering material, roof construction type and the level of insulation in
#' the roof/loft (depending on type of roof is present).
#'
#'@param  - combined sav files data.frame from the EHS data
roofs.make <- function(allEntries){
  roofs <- data.frame(
    aacode = allEntries$aacode,
    insulationthickness = loft.insulation.lookup(allEntries$loftinsx),
    coveringtype = roof.covering.type.lookup(allEntries$typercov),
    structuretype = roof.structure.type.lookup(allEntries$typerstr),
    contructiontype = roof.construction.type(allEntries$typercov, allEntries$typerstr)
  )
  
  print(paste("roofs DTO complete; number of records: ", nrow(roofs)))
  
  return (roofs)
}

#' ## Roof structure type
#'
#'Assign each dwelling case a roof structure
#'
#'@assumption - NHM doesn't have mixed type, pitched roofs are most common in the 
#'survey so map to this

roof.structure.type.lookup <- function(typerstr){
  as.factor(checked.revalue(
    typerstr,c(
      "mixed types" = "Pitched",
      "pitched" = "Pitched",
      "mansard" = "Mandard",
      "flat" = "Flat",
      "chalet" = "Chalet"
      )))
}

roof.structure <- function(typerstr){
  return (as.character(
    levels(roof.structure.lookup(typerstr))[roof.structure.lookup(typerstr)]))
}

roof.structure.lookup <- function(typerstr){ 
  as.factor(checked.revalue(
    typerstr,c(
      "chalet" = "PitchedSlateOrTiles",
      "pitched" = "PitchedSlateOrTiles",
      "mixed types" = "PitchedSlateOrTiles",
      "flat" = "Flat",
      "mansard" = "PitchedSlateOrTiles"
    )))
}

#' ##Roof covering type
#' 
#' Assign the type of roofing material (e.g. tiles, slates or other material) to each
#' dwelling case
#' 
#' @asummption - Where covering type is mixed types assume is actually Tile
roof.covering.type.lookup <- function(typercov){
  as.factor(checked.revalue(
    typercov,c(
      "mixed types" = "Tile",
      "natural slate/stone/shingle/thatch" = "Slates",
      "man made slate" = "Slates",
      "clay tile" = "Tile",
      "concrete tile" = "Tile",
      "asphalt" = "Asphalt",
      "felt" = "Felt",
      "glass/metal/laminate" = "Metal"
    )))
}

#' ##Roof construction type
#'
#'@param typercov - predominantTypeOfRoofCovering
#'@param typerstr - predominantTypeOfRoofStucture
roof.construction.type <- function(typercov, typerstr){
  constructionType <- ifelse(typercov == "thatch", "Thatched", NA)
  constructionType <- ifelse(is.na(constructionType), roof.structure(typerstr), 
                             constructionType)
  
  return (constructionType)
}



#' ## Loft insulation thickness
#' 
#' Loft insulation for properties withouta loft or heat loss roof can be left blank 
#' or set to 0. 

loft.insulation <- function(loftinsx){
    return (as.numeric(levels(
      loft.insulation.lookup(loftinsx))[loft.insulation.lookup(loftinsx)]))
}

loft.insulation.lookup <- function(loftinsx){
#     from.Flithick <- as.factor(checked.revalue(
#                       Flithick,c(
#                         "25mm" = 25,
#                         "50mm" = 50,
#                         "75mm" = 75,
#                         "100mm" = 100,
#                         "125mm" = 125,
#                         "150mm" = 150,
#                         "200mm" = 200,
#                         "250mm" = 250,
#                         "300mm" = 300,
#                         ">300mm" = 350,
#                         "No insulation" = 0,
#                         "Don t know thickness" = 0,
#                         "------------" = 0
#                         )))
    #override previous logic with loftinxs variable from derived physical file
    from.loftinsx <- ifelse(loftinsx < 12, 0,
                            ifelse(loftinsx == 12, 0,
                            ifelse(loftinsx <= 25, 25,
                            ifelse(loftinsx <= 50, 50,
                            ifelse(loftinsx < 90, 75,
                            ifelse(loftinsx < 113, 100,
                            ifelse(loftinsx < 138, 125,
                            ifelse(loftinsx < 175, 150,
                            ifelse(loftinsx < 225, 200,
                            ifelse(loftinsx < 275, 250,
                            ifelse(loftinsx < 325, 300,
                            ifelse(loftinsx >= 325, 350,
                            0))))))))))))
    ifelse(is.na(from.loftinsx), 0, from.loftinsx)
}
