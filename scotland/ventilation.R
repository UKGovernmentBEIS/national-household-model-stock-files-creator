#' ---
#' title: "Ventilation functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save ventilation data
#'
#'Create a .csv file using the function make.ventilation, which creates a dataframe 
#'containing a complete set of populated variables for the ventilation.csv stock 
#'file.
#'
#'@param shcs - the scottish survey data
#' 
#'@param output - the path to the output file including the required file name and
#' the extension .csv
save.ventilation <- function(shcs, output) {
  write.csv(make.ventilation(shcs), file=output, row.names=FALSE)
}


#'Make the dataframe that contains all the information required for the
#' ventilation.csv file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.ventilation <- function(shcs) {
  #Most columns are not used in the NHM but cannot be left blank. They are created as
  #vectors containing all 0 or "natural" of the same length as the number of cases 
  the.chimneysmainheating <- rep(0, length(shcs$uprn_new))
  the.chimneysother <- rep(0, length(shcs$uprn_new))
  the.chimneyssecondaryheating <- rep(0, length(shcs$uprn_new))
  the.intermittentfans <- rep(0, length(shcs$uprn_new))
  the.passivevents <- rep(0, length(shcs$uprn_new))
  the.ventilationsystem <- rep("natural", length(shcs$uprn_new))
  #Column is used in the NHM, however as no information is available it is 
  #based on the extent of double glazing -  this is consistant with previous stock
  #creation and the option to apply as described below.
  the.windowsanddoorsdraughtstrippedproportion <- 
    windows.anddoorsdraughtstrippedproportion(shcs$Q47) 
  
  data.frame(aacode = shcs$uprn_new
             ,chimneysmainheating = the.chimneysmainheating
             ,chimneysother = the.chimneysother
             ,chimneyssecondaryheating = the.chimneyssecondaryheating
             ,intermittentfans = the.intermittentfans
             ,passivevents = the.passivevents
             ,ventilationsystem = the.ventilationsystem
             ,windowsanddoorsdraughtstrippedproportion =
               the.windowsanddoorsdraughtstrippedproportion
  )
}

#'
#'\pagebreak
#'
#'##Draught stripped proportion
#'
#'Infer the proportion of window and door draught stripped from the proportion of
#' double glazing 
#'
#'There is no information available in the scottish survey and this value is 
#' inferred from the proportion of double glazing as described in the RD SAP 
#'  methodlogy. See S8.1:
#    if the state of the draught proofing cannot be determined then take triple, 
#    double or secondary glazed as being draught proofed, and single glazed windows
#    and doors as not draught stripped
#' 
#'@param draughtproof - the percentage of double glazing from the scottish survey
windows.anddoorsdraughtstrippedproportion<-function(draughtproof){
  draughtproof <- draughtproof/10
  return(draughtproof)
}
