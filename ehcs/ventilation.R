#' ---
#' title: "Ventilation functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE, warnings=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make ventilation data
#'
#'Create a dataframe containing a complete set of populated variables for the 
#'ventilation.csv stock file.
#'
#' @param allEntries - combined sav files data.frame from the EHS data
make.ventilation <- function(allEntries, elevationsDTO){
  
  #calculate the percentage draught stripped from the proportion of double glazed 
  #windows
  draftStipped <- proportion.draft.stripped(elevationsDTO)
  
  #NHM import used heating systems to determine, chimneys, vents and fans so we zero 
  # these in the ventilations file - they are no longer used.
  allVentilation <- data.table(
    aacode = allEntries$aacode
    ,chimneysmainheating = rep(0, nrow(allEntries))
    ,chimneyssecondaryheating = rep(0, nrow(allEntries))
    ,chimneysother = rep(0, nrow(allEntries))
    ,intermittentfans = rep(0, nrow(allEntries)) 
    ,passivevents = rep(0, nrow(allEntries))
    ,ventilationsystem = rep("natural", nrow(allEntries))
    )
  
  allVentilation <- join(allVentilation, draftStipped, by = "aacode")
  print(paste("ventilation DTO complete; number of records: ", nrow(allVentilation)))
  
  return(allVentilation)
}

#' ##Proportion of dwelling draught proofing 
#' 
#' Calculates proportion of Windows and Doors that are draft stripped as the mean
#' of percentage double glazed elevations (in line with RD SAP assumptions).
#'
#'@param elevationDTO - data.frame containing aacode and percentageWindowDblGlazed 
#'for each elevation
proportion.draft.stripped <- function(elevationsDTO){
  
  elevationDTOTable <- data.table(elevationsDTO)
  meanPercentDblGlazed <- elevationDTOTable[, j=list(mean(percentagedoubleglazed)), 
                                            by = aacode]
  draftStipped <- data.table(
    aacode = meanPercentDblGlazed$aacode
    ,windowsanddoorsdraughtstrippedproportion = meanPercentDblGlazed$V1 / 100
  )
  
  return (draftStipped)
}
