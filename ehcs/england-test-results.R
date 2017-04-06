#' ---
#' title: "England stock test report"
#' author: "Centre for Sustainable Energy"
#' output: pdf_document
#' toc: true
#' ---

#+ eval=TRUE, echo=FALSE,warning = FALSE, message = FALSE, comment = NA
#Read in all data required

library(Hmisc)
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

source("theme_cse.R")
source("test_functions.R")
source("main.R")

#england.outputs <- file.path(dirname(getwd()), "outputs","scotland")
#path.to.ehcs <- file.path(getwd(), "data/EHS_2014")

ehs <- merge.all.sav.files(path.to.ehcs)

cases <- read.csv(file.path(england.outputs,"cases.csv"), header = TRUE)
occupants <- read.csv(file.path(england.outputs,"occupants.csv"), header = TRUE)
lighting <- read.csv(file.path(england.outputs,"lighting.csv"), header = TRUE)
roofs <- read.csv(file.path(england.outputs,"roofs.csv"), header = TRUE)
elevations <- read.csv(file.path(england.outputs,"elevations.csv"), header = TRUE)
space_heating <- read.csv(file.path(england.outputs,"space-heating.csv"), 
                          header = TRUE)
water_heating <- read.csv(file.path(england.outputs,"water-heating.csv"), 
                          header = TRUE)


#'
#' \pagebreak
#'
#' #Stock comparison results
#' 
#' The following set of results have been produced by directly joining the original 
#' EHS survey data at case level to the outputs of the stock conversion process 
#' (i.e. the data contained in the csv stock files).
#' 
#' The data in a selection of original EHS variables and the NHM stock variables 
#' created through the stock conversion process have then been compared.
#' 
#' The tests group the original values/levels from the EHS with the correspoinding 
#' values at dwelling case level through the use of crosstabs.
#' 
#' In the tables below, the count column shows the number of cases in each row which 
#' have the original ehs.value *and* the corresponding final nhm.value. Essentially, 
#' the tests combining the pre and post stock processing information, and then report
#' on the frequencies of the combinations.
#' 
#' An Additional benefit of using this testing approach and presenting these results as
#' such, is to demonstrate some of the logic used to map the EHS to some of the NHM 
#' variables as they appear in the stock files.
#'
#'\pagebreak
#'
#' ##Cases
#' 

#' ###Built form
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.builtform(ehs, cases))
builtform <- data.frame(survey=ehs$dwtypenx,stock = cases$builtformtype)
ggplot(builtform,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=6))    


#' 
#' \pagebreak
#' 
#' ###Built year
#' 
#' Values have been banded in ten year intervals
#+ eval=TRUE, echo=FALSE
knitr::kable(test.builtyear(ehs, cases))

#' 
#' \pagebreak
#' 
#' ###Tenure
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.tenure(ehs, cases))
tenure <- data.frame(survey=ehs$tenure8x,stock = cases$tenuretype)
ggplot(tenure,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=3))   


#' 
#' \pagebreak
#' 
#' ###Region
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.region(ehs, cases))


#' 
#' \pagebreak
#' 
#' ###Morphology
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.rurality(ehs, cases))



#' 
#' \pagebreak
#' 
#' ###Habitable rooms
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.numberhabitablerooms(ehs, cases))

#' 
#' \pagebreak
#' 
#' ###Bedrooms
#' 
#' The test for bedrooms looks at a variable from the derived interview file (nBedsX)
#' which is not used in the stock conversion process - this uses a selection of 
#' room type variables in the introoms files, so the comparison is not a direct one, 
#' but indicative nevertheless.
#'  
#+ eval=TRUE, echo=FALSE
knitr::kable(test.numberbedrooms(ehs, cases))

#' 
#' \pagebreak
#' 
#' ###Ground floor type
#' 
#' Data in the ehs is ineffient to meaningfully test this. The test would use the 
#' exact same logic as the stock processing code so would not be a test as such. For 
#' the SHCS this was a more straightforward test.
#' 
#+ eval=TRUE, echo=FALSE
# knitr::kable(test.groundfloortype(ehs,cases))

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
knitr::kable(test.adults(ehs, cases))

#'
#' \pagebreak
#' 
#' ###Children
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.children(ehs, cases))

#'
#' \pagebreak
#' 
#' ###Income
#' 
#' Values banded in \pounds 5,000 intervals
#+ eval=TRUE, echo=FALSE
knitr::kable(test.income(ehs, occupants))

#' ##Roofs
#' 
#' Tests on roofs variables
#' 
#' ###Loft insulation
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.loftinsulationthickness(ehs, roofs))

#' 
#' \pagebreak
#' 
#' ###Roof type
#' 
#' Note: *mandard* is a standardised spelling error in the nhm, it refers to *mansard*
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.rooftype(ehs, roofs))
rooftype <- data.frame(survey=ehs$typerstr,stock = roofs$structuretype)
ggplot(rooftype,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=2))  

#' 
#' \pagebreak
#' 
#' ###Roof covering type
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.roofcovertype(ehs, roofs))
roofcovertype <- data.frame(survey=ehs$typercov,stock = roofs$coveringtype)
ggplot(roofcovertype,aes(x=stock,fill=survey))+geom_bar(stat="bin")+scale_fill_brewer(palette = "RdYlBu")+theme_cse()+guides(fill=guide_legend(nrow=2))  


#'
#' \pagebreak
#'  
#' ##Elevations
#' 
#' Tests on elevation variables
#' 
#' ###Wall construction
#'  
#+ eval=TRUE, echo=FALSE
knitr::kable(test.externalwallconstructiontype(ehs, 
                                        data.frame(subset
                                                   (elevations, 
                                                   elevations$elevationtype == "FRONT"))))

#'
#' \pagebreak
#'  
#' ###cavity wall insulation
#' 
#' The wallinsx variable in the survey are compared against the wall construction 
#' type and presence of cavity wall insulation of the front elevations described in 
#' the stock. Note that th ewallinsx variable is not used to determine whether a 
#' dwelling has cavity wall insulation. THis is calculated from data which specifies 
#' whether each face in turn has cavity wall insulation. 
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.cavitywallinsulation(ehs, 
                                       data.frame(subset
                                                  (elevations, 
                                                  elevations$elevationtype == "FRONT"))))



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
knitr::kable(test.spaceheatingsystemtype(ehs, space_heating))

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
knitr::kable(test.mainheatingfuel(ehs, space_heating))

#'
#' \pagebreak
#'  
#' ###Water heating system
#' 
#' The spaceheating system in the survey and the stock are compared. There will not be a direct match
#'  between the two due to boiler matching against the *SEDBUK* database and the systems described by
#'  *SAP* and the fuel types available in the NHM.
#' 
#+ eval=TRUE, echo=FALSE
knitr::kable(test.waterheatingsystemtype(ehs, water_heating))
