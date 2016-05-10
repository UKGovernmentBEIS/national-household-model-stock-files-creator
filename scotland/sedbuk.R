#' ---
#' title: "Sedbuk functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'This file creates the input file required for the java sedbuk heating lookup
#' program from the survey data
#'
#'Once the file is created by this script the sedbuk program must be used to create
#' the sedbuk input file required by spaceheating

#'Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'#Make and save sedbuk file for input into sedbuk program
#'
#'Function which runs the rest of the script, called by runsedbuk.R
#'
#'@param path.to.shcs - file path to SHCS SPSS file
#' 
#'@param path.to.output - directory where output files are put
make.sedbuk <- function(path.to.shcs, path.to.output) {
  print(paste("Loading", path.to.shcs))
  shcs <- read.spss(path.to.shcs, to.data.frame = TRUE, reencode='utf-8')
  
  sedbuk.path <- file.path(path.to.output, "sedbuk-input.csv")
  print(paste("Creating", sedbuk.path))
  save.sedbuk(shcs, sedbuk.path)
}

#'Create a .csv file that contains the correct data which is created in the function
#' create.sedbuk
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
save.sedbuk <- function(shcs, output) {
  write.csv(create.sedbuk(shcs), file=output, row.names=FALSE)
}

#'Make the dataframe that contains all the information required for the sedbuk input
#' file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
create.sedbuk <- function(shcs) {
  shcs <- only.completecases(shcs,"year")
  the.finmhfue = make.finmhfue(shcs$M5,shcs$L2)
  the.finmhboi = make.finmhboi(shcs$M7)
  the.finchbcd = make.finchbcd(shcs$uprn_new)
  the.finwhxty = make.finwhxty(shcs$M17,shcs$M18)
  
  
  data.frame(aacode = shcs$uprn_new
             ,Finmhfue = the.finmhfue
             ,Finmhboi = the.finmhboi
             ,Finchbcd = the.finchbcd
             ,Finchbma = shcs$M10a
             ,Finchbmo = shcs$M10b
             ,Finwhxty = the.finwhxty
  )
}

#'
#'\pagebreak
#'
#'#Required vectors
#'
#'Complete cases
#'
#'The sedbuk file is created only cases where the check is true
#' 
#'@param data - the scottish survey data
#'
#'@param checks - the year from scottish survey data
only.completecases <- function(data, checks) {
  completeVec <- complete.cases(data[, checks])
  return(data[completeVec, ])
}

#'finmhfue
#'
#'Create the main heating fuel from the main heating fuel and the meter type
#'
#'@param fuel - main heating fuel from scottish survey
#'
#'@param meter - electric tariff meter from scottish survey
make.finmhfue <- function(fuel,meter){
  finmhfue <- as.factor(checked.revalue(
    fuel,
    c("Gas (mains)"	=	"Gas_Mains"
      ,"Bulk LPG"	=	"Gas_Bulk_LPG"
      ,"Bottled gas"	=	"Gas_Bottled"
      ,"Oil"	=	"Oil"
      ,"House coal"	=	"SolidFuel_Coal"
      ,"Smokeless fuel"	=	"SolidFuel_SmokelessFuel"
      ,"Antracite nuts and grain"	=	"SolidFuel_Anthracite"
      ,"Wood chips"	=	"SolidFuel_Wood"
      ,"Wood logs"	=	"SolidFuel_Wood"
      ,"Wood pellets"	=	"SolidFuel_Wood"
      ,"Peak electric"	=	"Electricity_Standard"
      ,"Off-peak electric"	=	"Electricity_7HrTariff"
      ,"Communal heating, no CHP"	=	"Communal_FromBoiler"
      ,"Communal heating, with CHP"	=	"Communal_CHP_WasteHeat"
      ,"Biogas"	=	"__MISSING"
      ,"Dual fuel"	=	"SolidFuel_SmokelessFuel"
      ,"Other"	=	"__MISSING"
      ,"Not applicable"=		"__MISSING"
      ,"Unobtainable"=	"__MISSING")))
  levels(finmhfue) <- c(levels(finmhfue), "Electricity_24HrTariff")
  finmhfue[meter=="24 hour"] <- "Electricity_24HrTariff"
    return(finmhfue)
}

#'finmhboi
#'
#'Create the boiler type
#'
#'@param boilertype - boilertype from scottish survey
make.finmhboi <- function(boilertype){
  finmhboi <- as.factor(checked.revalue(
    boilertype,
    c("Standard" = "Standard"
      ,"Combi" = "Combination"
      ,"Condensing standard" = "Condensing"
      ,"Condensing Combi" = "CondensingCombi"
      ,"Back Boiler" = "BackBoiler"
      ,"CPSU" = "CombinedPrimaryStorageUnit"
      ,"Range" = "NoBoiler"
      ,"Not applicable" = "NoBoiler"
      ,"Unobtainable" = "Unknown"
    )))
  return(finmhboi)
}

#'finchbcd
#'
#'Create the boilercode
#'
#'@param boilercode - boiler code does not exist in the scottish survey, a vector
#' with 9999 to indicate unknown is created
make.finchbcd <- function(boilercode){
  boilercode <- 9999
  return(boilercode)
}

#'finwhxty
#'
#'Create the waterheating fuel
#'
#'@param hotwatersource - type of waterheating from scottish survey
#'
#'@param hotwaterfuel - main hot water fuel from scottish survey
make.finwhxty <- function(hotwatersource,hotwaterfuel){
  finwhxty <- as.factor(checked.revalue(
    hotwaterfuel,
    c("Gas (mains)" =	"MainsGas"
      ,"Bulk LPG" =	"BulkLPG"
      ,"Bottled gas" =	"__MISSING"
      ,"Oil" =	"Oil"
      ,"House coal" =	"Coal"
      ,"Smokeless fuel" =	"Smokeless"
      ,"Antracite nuts and grain" =	"Anthracite"
      ,"Wood chips" =	"Wood"
      ,"Wood logs" =	"Wood"
      ,"Wood pellets" =	"Wood"
      ,"Peak electric" =	"__MISSING"
      ,"Off-peak electric" =	"__MISSING"
      ,"Communal heating, no CHP" =	"__MISSING"
      ,"Communal heating, with CHP" =	"__MISSING"
      ,"Biogas" =	"__MISSING"
      ,"Dual fuel" =	"__MISSING"
      ,"Other" =	"__MISSING"
      ,"Not applicable" =	"__MISSING"
      ,"Unobtainable" =	"__MISSING"
    )))
  finwhxty[hotwatersource != "Room heater BB"] <- "__MISSING"
  return(finwhxty)
}
