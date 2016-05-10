library(foreign)
library(memisc)
library(Hmisc)
library(plyr)
library(dplyr)
library(stats)
library(reshape)
library(classInt)


source("functions-for-raw-wales-data-production.R", chdir = TRUE)

# set location of spss data for LIW 2008 files
liw.physical.data.location <- file.path("S:/R&A/Team Resources/DATASETS/Living in Wales Survey/2008 Property Survey/DATA/SPSS Files")
liw.social.data.location <- file.path("S:/R&A/Team Resources/DATASETS/Living in Wales Survey/2008 Social Survey/DATA/SPSS Files")
##set output location
liw.analysis.outputs <- file.path(getwd(), "data/LiW-2008")

#produce wales stock creation programme input data
summarise.wales.data(liw.physical.data.location, liw.social.data.location, liw.analysis.outputs)




#view recoding results if required...
#recode.report(liw.data$builtformtype)
#recode.report(liw.data$morphologytype)
#recode.report(liw.data$externalwallconstructiontype)
#recode.report(liw.data$cavitywallinsulation)
#recode.report(liw.data$insulationthickness)
#recode.report(liw.data$numofbedrooms)
#recode.report(liw.data$numofhabitalrooms)
#recode.report(liw.data$tenuretype)
#recode.report(liw.data$adults)
#recode.report(liw.data$children)
#recode.report(liw.data$hasdisabledorsickoccupant)
#recode.report(liw.data$buildyear)
#recode.report(liw.data$householdincomebeforetax)
#recode.report(liw.data$chiefincomeearnersage)
