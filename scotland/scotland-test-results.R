#' ---
#' title: "Scotland survey stock test report"
#' author: "Centre for Sustainable Energy"
#' output: pdf
#' toc: true
#' ---

#+ child=c('scottishsurveyprocessing.R','theme_cse.R','test_functions.R')

#+ eval=TRUE, echo=FALSE,warning = FALSE, message = FALSE, comment = NA
#Read in all data required
source("theme_cse.R")
source("test_functions.R")
scotland.outputs <- file.path(dirname(getwd()), "outputs","scotland")
scotland.survey <- file.path(dirname(getwd()), "data","SHCS_11-13","external_cse_data.sav")
shcs <- read.spss(scotland.survey, to.data.frame = TRUE, reencode='utf-8')
shcs <- clean.survey(shcs)
cases <- read.csv(file.path(scotland.outputs,"cases.csv"), header = TRUE)
occupants <- read.csv(file.path(scotland.outputs,"occupants.csv"), header = TRUE)
lighting <- read.csv(file.path(scotland.outputs,"lighting.csv"), header = TRUE)
roofs <- read.csv(file.path(scotland.outputs,"roofs.csv"), header = TRUE)
elevations <- read.csv(file.path(scotland.outputs,"elevations.csv"), header = TRUE)
space_heating <- read.csv(file.path(scotland.outputs,"space-heating.csv"), 
                          header = TRUE)

#'
#' \pagebreak
#'
#' #Stock comparison results
#' 
#' The following set of results have been produced by directly joining the original 
#' SHCS survey data at case level to the outputs of the stock conversion process 
#' (i.e. the data contained in the csv stock files).
#' 
#' The data in a selection of original SHCS variables and the NHM stock variables 
#' created through the stock conversion process have then been compared.
#' 
#' The tests group the original values/levels from the SHCS with the correspoinding 
#' values at dwelling case level through the use of crosstabs.
#' 
#' In the tables below, the count column shows the number of cases in each row which 
#' have the original shcs.value *and* the corresponding final nhm.value. Essentially, 
#' the tests combining the pre and post stock processing information, and then report
#' on the frequencies of the combinations.
#' 
#' Additional bonus of using this testing approach and presenting these results as
#' such, is to demonstrate some of the logic used to map the SHCS to some of the NHM 
#' variables as they appear in the stock files.
#'
#' ##Cases
#' 

#' ###Built form
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.builtform(shcs, cases))
builtform <- data.frame(survey=paste(shcs$C1,shcs$C2,sep=" - "),stock = cases$builtformtype)
ggplot(builtform,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=6))    


#' 
#' \pagebreak
#' 
#' ###Built year
#' 
#' Values have been banded in ten year intervals
#+ eval=TRUE, echo=FALSE
knitr::kable(test.builtyear(shcs, cases))

#' 
#' \pagebreak
#' 
#' ###Tenure
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.tenure(shcs, cases))
tenure <- data.frame(survey=shcs$tenure,stock = cases$tenuretype)
ggplot(tenure,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=3))   


#' 
#' \pagebreak
#' 
#' ###Region
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.region(shcs, cases))


#' 
#' \pagebreak
#' 
#' ###Morphology
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.rurality(shcs, cases))



#' 
#' \pagebreak
#' 
#' ###Habitable rooms
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.numberhabitablerooms(shcs, cases))


#' 
#' \pagebreak
#' 
#' ###Bedrooms
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.numberbedrooms(shcs, cases))

#' 
#' \pagebreak
#' 
#' ###Ground floor type
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.groundfloortype(shcs,cases))
floortype <- data.frame(survey=shcs$N1_E,stock = cases$grndfloortype)
ggplot(floortype,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=3))   


#'
#' \pagebreak
#'  
#' ##Occupants
#' 
#' Tests on occupants variables
#' 
#' ###Adults
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.adults(shcs, cases))

#'
#' \pagebreak
#' 
#' ###Children
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.children(shcs, cases))

#'
#' \pagebreak
#' 
#' ###Income
#' 
#' Values banded in \pounds 5,000 intervals
#+ eval=TRUE, echo=FALSE
knitr::kable(test.income(shcs, occupants))

#'
#' \pagebreak
#'  
#' ##Lighting
#' 
#' Tests on lighting variables
#' 
#' ###Low energy lightbulbs
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.lighting(shcs, lighting))

#'
#' \pagebreak
#'  
#' ##Roofs
#' 
#' Tests on roofs variables
#' 
#' ###Loft insulation
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.loftinsulationthickness(shcs, roofs))

#' 
#' \pagebreak
#' 
#' ###Roof type
#' 
#' Note: *mandard* is a standardised spelling error in the nhm, it refers to *mansard*
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.rooftype(shcs, roofs))
rooftype <- data.frame(survey=shcs$Q19,stock = roofs$structuretype)
ggplot(rooftype,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=2))  

#'
#' \pagebreak
#'  
#' ##Elevations
#' 
#' Tests on elevation variables
#' 
#' ###Wall construction
#' 
#' The wall type of the front elevation is compared to the *primary* wall type described
#'  in the survey. Some cases have a different wall type described for the stock than that
#'  of the *primary* wall type. This is because the extent of the *primary* wall type
#'  is described in the survey as less than 5/10 of a wall and so the *secondary* wall type is applied.
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.externalwallconstructiontype(shcs, 
                                        data.frame(subset
                                                   (elevations, 
                                                   elevations$elevationtype == "front"))))

#'
#' \pagebreak
#'  
#' ###Wall insulation
#' 
#' The *primary* wall type and insulation described in the survey are compared against 
#'  that of the front elevations described in the stock.
#' 
#+ eval=TRUE, echo=FALSE
elevations$cavitywallinsulation[is.na(elevations$cavitywallinsulation)==TRUE] <-"NULL"
elevations$internalinsulation[is.na(elevations$internalinsulation)==TRUE] <-"NULL"
elevations$externalwallinsulation[is.na(elevations$externalwallinsulation)==TRUE] <-"NULL"

elevations$cavitywallinsulation <- as.factor(elevations$cavitywallinsulation)
elevations$internalinsulation <- as.factor(elevations$internalinsulation)
elevations$externalwallinsulation <- as.factor(elevations$externalwallinsulation)

wallinsulation <- data.frame(subset(elevations, 
                                    elevations$elevationtype == "front"))
wallinsulation <- data.frame(aacode = wallinsulation$aacode
                             , wallandinsulation = 
                               paste
                             (externalwallconstructiontype 
                             = wallinsulation$externalwallconstructiontype
                             , cavitywallinsulation = as.factor(checked.revalue(
                               wallinsulation$cavitywallinsulation,c(
                                 "TRUE" = "CWI",
                                 "FALSE" = "",
                                 "NULL" = "No wall")))
                             , internalwallinsulation = as.factor(checked.revalue(
                               wallinsulation$internalinsulation,c(
                                 "TRUE" = "IWI",
                                 "FALSE" = "",
                                 "NULL" = "No wall")))
                             , externalwallinsulation = as.factor(checked.revalue(
                               wallinsulation$externalwallinsulation,c(
                                 "TRUE" = "EWI",
                                 "FALSE" = "",
                                 "NULL" = "No wall"))), sep = " "))
knitr::kable(test.externalwalltypeandinsulation(shcs, wallinsulation))

#'
#' \pagebreak
#'  
#' ##Spaceheating
#' 
#' Tests on spaceheating variables
#' 
#' ###Spaceheating system
#' 
#' The spaceheating system in the survey and the stock are compared. There will not be a direct match
#'  between the two due to boiler matching against the *SEDBUK* database and the systems described by
#'  *SAP*.
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.spaceheatingsystemtype(shcs, space_heating))

#'
#' \pagebreak
#'  
#' ###Spaceheating fuel
#' 
#' The spaceheating system in the survey and the stock are compared. There will not be a direct match
#'  between the two due to boiler matching against the *SEDBUK* database and the systems described by
#'  *SAP* and the fuel types available in the NHM.
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.mainheatingfuel(shcs, space_heating))
