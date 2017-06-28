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
									                  lenres2=allEntries$lenres2,		#CMT added asleresis onyl available for half the years. Eventualy we wat a proper fix which maps lenresb
									                  AWEligible = NA, #allEntries$ECO_AW,	#CMT Needs tobe changedt map the various values to either TRUE or FALSE #TB - variable does not exist
                                    hhcompx = allEntries$hhcompx,
                                    imd1010 = allEntries$imd1010,   #CMT - corrected spelling to EHS 2014 data for imd1010 variable in allEntries df - changed i to lower case
                                    wallinsy = allEntries$wallinsy,  #CMT - replaced wallinsx with wallinsy, the equivalent variable in EHS2014
                                    Felroofp = allEntries$felroofp,
                                    CERTpriority = allEntries$CERTpriority,
                                    FloorArea = allEntries$FloorArea.x, #CMT additional variable (from dimensions file)
                                    FloorArea_Addnl_conservatory = allEntries$fincosiz,
                                    GF_area = NA, #CMT adding in SHCS floor area values in (GF) TO MATCH SCOTLAND
                                    f1st_area = NA, #CMT adding in SHCS floor area values in (1st) TO MATCH SCOTLAND
                                    s2nd_area= NA, #CMT adding in SHCS floor area values in (2nd) TO MATCH SCOTLAND
                                    t3rd_area = NA, #CMT adding in SHCS floor area values in (3rd+) TO MATCH SCOTLAND
                                    RiR_area = NA, #CMT adding in SHCS floor area values in (R-I-R) TO MATCH SCOTLAND
                                    Ext1_area = NA, #CMT adding in SHCS floor area values in (extension)  TO MATCH SCOTLAND
                                    Ext2_area = NA, #CMT adding in SHCS floor area values in (extension 2)  TO MATCH SCOTLAND
                                    WFG_preApr11 = allEntries$WFG_preApr11,
                                    FPBasicIncomeDef = allEntries$fpflgb,  #CMT additional variable
                                    FPFullIncomeDef = allEntries$fpflgf,  #CMT additional variable
                                    sap09 = NA,
                                    sap12 = allEntries$sap12  #CMT - replaced by sap12 given sap09 variable nolonger exist in EHS 2014.
                                   )
  print(paste("additional-properties DTO complete; number of records: ", 
              nrow(additional.properties)))
  return(additional.properties)
}
