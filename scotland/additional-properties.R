#' ---
#' title: "Additional-properties functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'##Make and save additional-properties data
#'Create a .csv file using the function make.additionalproperties, which creates a 
#'dataframe containing a complete set of populated variables for the additional-prope
#'rties.csv stock file.
#'
#'This file must be present in the stock import DTO package but can be empty save for
#'the aacode field containing the survey code. However, additional field can be added
#'to the file that can be called from scenarios in the NHM.
#'
#'Currently, the code here populates the additional-properties.csv file with seven 
#'fields. These are SHCS raw variables and a corresponding renamed and recoded 
#'variable to match variables and levels contained in the ehs. These recoded 
#'variables are findisty (from  M3), hhcompx(from hhtype), dblglaz4 (from Q47) and 
#'wallinsx from five SHCS varieable describing the primary and secondary wall types.
#'(these are not currently included in the file.)
#'
#' @param shcs - the scottish survey data
#' 
#' @param output - the path to the output file including the required file name and
#' the extension .csv
save.additionalproperties <- function(shcs, output) {
  #creates a completed .csv file from:
  #write.csv(dataframe containing values and column names, file name, without row id)
  write.csv(make.additionalproperties(shcs), file=output, row.names=FALSE, na="")
}

#'Make the dataframe that contains all the information required for the additional-
#'properties.csv file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#'
#'Additional columns can be added to the data frame by adding additional column names
#' and vectors to fill that column (see help(data.frame))
#'
#' @param shcs - the scottish survey data
make.additionalproperties <- function(shcs) {
  #creates a dataframe containing column names (word before the = sign) and a vector
  #to fill that column (vector after the = sign)
    shcs.findisty <- get.findisty(shcs$M3) 
    shcs.hhcompx <- get.hhcompx(shcs$hhtype)
    shcs.dblglaz4 <- get.dblglaz4(shcs$Q47)
    shcs.wallinsx <- get.wallinsx(shcs$Q2, shcs$Q8, shcs$Q10, shcs$Q11, shcs$Q17)
    shcs.Felcav <- get.Felcav(shcs$Q8, shcs$Q10, shcs$Q17)
    shcs.Felext <- get.Felext(shcs$Q8, shcs$Q10, shcs$Q17)
    shcs.Felpv <- get.Felpv(shcs$D8)
    shcs.Felsol <- get.Felsol(shcs$D9)

  data.frame(aacode = shcs$uprn_new,
            Farnatur = NA,
            Felorien = NA,
            Felcavff = NA,
            Felcavlf = NA,
            Felcavrf = NA,
            Felcavbf = NA,
            Felextff = NA,
            Felextlf = NA,
            Felextrf = NA,
            Felextbf = NA,
            Felpvff = NA,
            Felpvlf = NA,
            Felpvrf = NA,
            Felpvbf = NA,
            Felsolff = NA,
            Felsollf = NA,
            Felsolrf = NA,
            Felsolbf = NA,
            Felcav_shcs = shcs.Felcav,
            Felext_shcs = shcs.Felext,
            Felpv_shcs = shcs.Felpv,
            Felsol_shcs = shcs.Felsol,
            Findisty = shcs.findisty,
            dblglaz4 = shcs.dblglaz4,
            NRmsEHS = shcs$J1,
            NRms2a = NA,
            NRms4 = NA,
            NRms5 = NA,
			lenres2= NA,	#CMT addedtoenale GB stock to knit together
			AWEligible = NA,	#CMT addedtoenale GB stock to knit together
            hhcompx = shcs.hhcompx,
            imd1010 = NA,
            wallinsy = shcs.wallinsx, #CMT - replaced wallinsx with wallinsy, the equivalent variable in EHS2014 (to match England)
            Felroofp = NA,
            CERTpriority = NA,
            FloorArea = NA, #CMT additional variable (to match England)
            FloorArea_Addnl_conservatory = NA,
            GF_area = shcs$N1_A, #CMT adding in SHCS floor area values in (GF)
            f1st_area = shcs$N2_A, #CMT adding in SHCS floor area values in (1st)
            s2nd_area = shcs$N3_A, #CMT adding in SHCS floor area values in (2nd)
            t3rd_area = shcs$N4_A, #CMT adding in SHCS floor area values in (3rd+)
            RiR_area = shcs$N5_A, #CMT adding in SHCS floor area values in (R-I-R)
            Ext1_area = shcs$N6_A, #CMT adding in SHCS floor area values in (extension)
            Ext2_area = shcs$N7_A, #CMT adding in SHCS floor area values in (another extension)
            WFG_preApr11 = NA,
            FPBasicIncomeDef = NA,  #CMT additional variable in England - added to match
            FPFullIncomeDef = NA,  #CMT additional variable in England - added to match
            sap09 = shcs$SAP2009_BRE,
            sap12 = NA  #CMT - additional variable in England - added to match
  )
}

#' Map M3 to findisty; determine what distribution method is used by the main heating
#' system
get.findisty <- function(M3) {
  as.factor(checked.revalue(
    M3,
    c(
      "Radiators" = "Radiators",
      "Appliance" = "Question Not Applicable",
      "Under-floor" = "Underfloor",
      "Ceiling" = "Question Not Applicable",
      "Other" = "Question Not Applicable",
      "No wet system" = "Question Not Applicable",
      "Unobtainable" = "Unknown")))
}

#' 
get.hhcompx <- function(hhtype) {
  as.factor(checked.revalue(
    hhtype,
    c(
      "Single adult" = "one person under 60",
      "Small adult" = "couple, no dependent child(ren) under 60",
      "Single parent" = "lone parent with dependent child(ren)",
      "Small family" = "couple with dependent child(ren)",
      "Large family" = "couple with dependent child(ren)",
      "Large adult" = "other multi-person households",
      "Older smaller" = "couple, no dependent child(ren) aged 60 or over",
      "Single pensioner" = "one person aged 60 or over"
    )))
}


#' @return a factor, with levels "no double glazing" "less than half" "more than 
#' half" "entire house"
get.dblglaz4 <- function(Q47) {
  dblglaz4 <- cut(Q47, c(0, 1, 4, 9, 10, 100),
      labels = c(
      "no double glazing",
      "less than half",
      "more than half",
      "entire house",
      "unknown"))
  dblglaz4[is.na(dblglaz4)] <- "unknown"
  dblglaz4[dblglaz4 == "unknown"] <- "no double glazing"
  droplevels(dblglaz4)
}

#' 'wallinsx' variable creation:

#' Q2	- Primary external wall construction
#' Q8 - Insulation added to prim ext walls?
#' Q10 - Extent of prim external wall
#' Q11 - Sec external wall construction
#' Q17 - Insulation added to sec ext walls?

#' @return a factor, with levels "cavity insulated" "cavity uninsulated" "other"
get.wallinsx <- function(Q2, Q8, Q10, Q11, Q17) {
  # a column, which is true if we want the primary wall
  primary.wall <- Q10 >= 5
  
  construction <- ifelse(primary.wall, levels(Q2)[Q2], levels(Q11)[Q11])
  insulation   <- ifelse(primary.wall, levels(Q8)[Q8], levels(Q17)[Q17])
  
  ifelse(construction == "Cavity",
         # is it insulated?
         ifelse(insulation %in% "cavity",
                "cavity with insulation", "cavity uninsulated"),
         "other")
}

get.Felcav <- function(Q8, Q10, Q17) {
  primary.cwi <- ifelse(Q8 %in% "cavity", TRUE, FALSE)
  primary.Felcav <- ifelse(primary.cwi == TRUE, Q10, 0)
  secondary.cwi <- ifelse(Q17 %in% "cavity", TRUE, FALSE)
  secondary.Felcav <- ifelse(secondary.cwi == TRUE, 10 - Q10, 0)
  Felcav = primary.Felcav + secondary.Felcav
  Felcav <- ifelse(Felcav > 0, "Yes", "No")
}

get.Felext <- function(Q8, Q10, Q17) {
  primary.ext <- ifelse(Q8 %in% "External", TRUE, FALSE)
  primary.Felext <- ifelse(primary.ext == TRUE, Q10, 0)
  secondary.ext <- ifelse(Q17 %in% "External", TRUE, FALSE)
  secondary.Felext <- ifelse(secondary.ext == TRUE, 10 - Q10, 0)
  Felext = primary.Felext + secondary.Felext
  Felext <- ifelse(Felext > 0, "Yes", "No")
}

get.Felpv <- function(D8) {
  ifelse(D8 > 0 & D8 <= 10, "Yes", "No")
}

get.Felsol <- function(D9) {
  ifelse(D9 > 0 & D9 <= 10, "Yes", "No")
}

