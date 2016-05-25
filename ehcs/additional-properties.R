#' ---
#' title: "Additional-properties functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'##Make additional-properties data
#'Create a .csv file using the function make.additionalproperties, which creates a 
#'dataframe containing a complete set of populated variables for the additional-prope
#'rties.csv stock file.
#'
#'This file must be present in the stock import DTO package but can be empty save for
#'the aacode field containing the survey code. However, additional field can be added
#'to the file that can be called from scenarios in the NHM.
#'
#'Here a selection of variables of interest are selected in to a data.frame that is 
#'saved as a csv file as part of the make.england function. 
#'
#' @param allEntries - the merged collection of all SPSS files used in the stock 
#' conversion process.


make.additionalproperties <- function(allEntries) {
  #creates a dataframe called additional.properties containing column names (word 
  #before the = sign) and a vector to fill that column (vector after the = sign)  
  additional.properties <- data.table(aacode = allEntries$aacode,
                                    Farnatur = allEntries$Farnatur,
                                    Felorien = allEntries$felorien,
                                    Felcavff = allEntries$Felcavff,
                                    Felcavlf = allEntries$Felcavlf,
                                    Felcavrf = allEntries$Felcavrf,
                                    Felcavbf = allEntries$Felcavbf,
                                    Felextff = allEntries$Felextff,
                                    Felextlf = allEntries$Felextlf,
                                    Felextrf = allEntries$Felextrf,
                                    Felextbf = allEntries$Felextbf,
                                    Felpvff = allEntries$Felpvff,
                                    Felpvlf = allEntries$Felpvlf,
                                    Felpvrf = allEntries$Felpvrf,
                                    Felpvbf = allEntries$Felpvbf,
                                    Felsolff = allEntries$Felsolff,
                                    Felsollf = allEntries$Felsollf,
                                    Felsolrf = allEntries$Felsolrf,
                                    Felsolbf = allEntries$Felsolbf,
                                    Felcav_shcs = NA,
                                    Felext_shcs = NA,
                                    Felpv_shcs = NA,
                                    Felsol_shcs = NA,
                                    Findisty = allEntries$Findisty,
                                    dblglaz4 = allEntries$dblglaz4,
                                    NRmsEHS = allEntries$NRmsEHS,
                                    NRms2a = allEntries$NRms2a,
                                    NRms4 = allEntries$NRms4,
                                    NRms5 = allEntries$NRms5,
                                    hhcompx = allEntries$hhcompx,
                                    imd1010 = allEntries$Imd1010,
                                    wallinsx = allEntries$wallinsx,
                                    Felroofp = allEntries$felroofp,
                                    CERTpriority = allEntries$CERTpriority,
                                    WFG_preApr11 = allEntries$WFG_preApr11,
                                    sap09 = allEntries$sap09
                                   )
  print(paste("additional-properties DTO complete; number of records: ", 
              nrow(additional.properties)))
  return(additional.properties)
}
