#' ---
#' title: "Occupants functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save occupants data
#'
#'Create a .csv file using the function make.occupants, which creates a dataframe 
#'containing a complete set of populated variables for the occupants.csv stock 
#'file.
#'
#'The information on occupants covers income, chief income earner's age, chief income
#'earner's working hours, details on whether anyone in the dwelling is long term sick
#'or disabled, whether anyone in the household is one benefits and how long the 
#'current inhabitants have lived in the dwelling. Not all this information is 
#'required and some variables can be left NULL/blank.
#'
#' @param shcs - the scottish survey data
#' 
#' @param output - the path to the output file including the required file name and
#' the extension .csv
save.occupants <- function(shcs, output) {
  write.csv(make.occupants(shcs), file=output, row.names=FALSE, na = "")
}

#'Make the dataframe that contains all the information required for the occupants.csv
#' file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.occupants <- function(shcs) {
  the.datemovedin <- date.movedin(shcs$uprn_new)
  the.hasdisabledorsickoccupant <- has.disabledorsickoccupant(shcs$health)
  the.hasoccupantonbenefits <- has.occupantonbenefits(shcs)
  the.workinghours <- working.hours(shcs$uprn_new)
  
  data.frame(aacode = shcs$uprn_new
             ,chiefincomeearnersage = shcs$hihage
             ,datemovedin = the.datemovedin
             ,hasdisabledorsickoccupant = the.hasdisabledorsickoccupant
             ,hasoccupantonbenefits = the.hasoccupantonbenefits
             ,householdincomebeforetax = shcs$annhhinc
             ,workinghours = the.workinghours
  )
}

#'
#' \pagebreak
#'  
#'##Occupant information
#'
#'Make hasdisabledorsickoccupant
#'
#'This is mapped directly from a column in the scottish survey
#' 
#'@param occupant - health (any long term sick/disabled in household) from scottish
#' survey
has.disabledorsickoccupant <- function(occupant){
  as.factor(checked.revalue(
    occupant,
    c("No" = "FALSE"
      ,"Yes" = "TRUE"
      ,"Unobtainable" = "NULL")))
}


#'Make datemovedin
#'
#'There is no information in the scottish survey about the date moved in or length
#' of residency. This column cannot be blank and so is set as zero.
#' 
#' This variable uses Unix time - a system for describing instants in time, defined 
#' as the number of seconds that have elapsed since 00:00:00 Coordinated Universal 
#' Time (UTC), Thursday, 1 January 1970. It is used widely in Unix-like and many 
#' other operating systems and file formats.
#' 
#' Hence, by setting this variable to 0, all lengths of residency are state as being 
#' since 01/01/1970. However, this variable and the information do not form a core 
#' part of the stock data or how the stock is handle in the NHM and this will not 
#' have any direct impact on running scenarios in the model.
#'
#'@param moving - uprn_new from the scottish survey is used so that that the vector
#' created is of the correct length
date.movedin <- function(moving){
  moving <- as.data.frame(moving)
  moving[1] <- 0
  return(moving$moving)
}

#' Make workinghours
#'
#'There is no information in the scottish survey about the working hours of the
#' occupants. This column cannot be blank and so is set as "N".
#'
#'@param working - uprn_new from the scottish survey is used so that that the vector
#' created is of the correct length
working.hours <- function(working){
  working <- as.data.frame(working)
  working[1] <- "N"
  return(working$working)
}

#'
#' \pagebreak
#'  
#'##Benefits information
#'
#'Make hasoccupantsonbenefits
#'
#'Information on benefits is recorded across a large number of columns - each type
#' of benefit is recorded in a seperate column and are specific to the year in
#' which the survey was carried out.
#'
#'Benefits 2011: Note - jobseekers (income based) and jobseekers (contribution
#' based) are not included as no survey entries have been recorded and their 
#' inclusion introduces invalid entries to the data.
#'               
#'@param benefits - the whole of the scottish survey data (benefit data is spread
#' across a large number of columns)
has.occupantonbenefits <- function(benefits){
  #Benefits of interest from 2011 year are checked, if occupant recieved any of the
  #benefits listed then they are flagged as "TRUE" or having an occupant on benefits
  benefits$benefit2011<-
    ifelse( benefits$hh57_01_2011 == "Income support" 
            | benefits$hh57_02_2011 == "Working Tax Credit (WTC)"
            | benefits$hh57_03_2011 == "Child tax Credit (CTC)"
            | benefits$hh57_06_2011 == "Housing Benefit"
            | benefits$hh57_08_2011 == "Child Benefit"
            | benefits$hh57_12_2011 == "Pension Credit"
            | benefits$hh58_01_2011 ==
              "Incapacity Benefit (formerly Invalidity Benefit)"
            | benefits$hh58_02_2011 == "Disability Living Allowance Care component"
            | benefits$hh58_03_2011 ==
              "Disability Living Allowance Mobility Component"
            | benefits$hh58_06_2011 == "Severe Disablement Allowance"
            | benefits$hh58_09_2011 ==
              "A disability premium with your Income Support/Housing Benefi"
            | benefits$hh58_10_2011 == "Attendance Allowance"
            ,"TRUE"
            ,"FALSE")
  
  #Benefits of interest from 2012 year are checked, if occupant recieved any of the
  #benefits listed then they are flagged as "TRUE" or having an occupant on benefits
  benefits$benefit2012<-
    ifelse( benefits$hh57_01_2012 == "A - Income Support"
            | benefits$hh57_02_2012 == "B - Employment and Support Allowance" 
            | benefits$hh57_03_2012 == "C - Working Tax Credit (WTC)"
            | benefits$hh57_04_2012 == "D - Child Tax Credit (CTC)"
            | benefits$hh57_05_2012 ==
              "E - Jobseeker's Allowance (JSA) - Income Based"
            | benefits$hh57_06_2012 ==
              "F - Jobseeker's Allowance (JSA) - Contribution Based"
            | benefits$hh57_07_2012 == "G - Housing Benefit"
            | benefits$hh57_08_2012 == "H - Council Tax Benefit"
            | benefits$hh57_10_2012 == "J - Child Benefit"
            | benefits$hh57_14_2012 == "N - Pension Credit"
            | benefits$hh58_01_2012 ==
              "A - Incapacity Benefit (formerly Invalidity Benefit)"
            | benefits$hh58_02_2012 ==
              "B - Disability Living Allowance Care component"
            | benefits$hh58_03_2012 ==
              "C - Disability Living Allowance Mobility Component"
            | benefits$hh58_06_2012 == "F - Severe Disablement Allowance"
            | benefits$hh58_09_2012 ==
              "I - Disability premium with your Income Support/Housing Benefit"
            | benefits$hh58_10_2012 == "J - Attendance Allowance"
            ,"TRUE"
            ,"FALSE")
  
  #Benefits of interest from 2013 year are checked, if occupant recieved any of the
  #benefits listed then they are flagged as "TRUE" or having an occupant on benefits
  benefits$benefit2013<-
    ifelse( benefits$hh57_01_2013 == "A - Income Support"
            | benefits$hh57_02_2013 == "B - Employment and support allowance"
            | benefits$hh57_03_2013 == "C - Working Tax Credit (WTC)"
            | benefits$hh57_04_2013 == "D - Child Tax Credit (CTC)"
            | benefits$hh57_05_2013 ==
              "E - Jobseeker's Allowance (JSA) - Income Based"
            | benefits$hh57_06_2013 ==
              "F - Jobseeker's Allowance (JSA) - Contribution Based"
            | benefits$hh57_07_2013 == "H - Housing Benefit"
            | benefits$hh57_08_2013 == "I - Council Tax Benefit"
            | benefits$hh57_10_2013 == "K - Child Benefit"
            | benefits$hh57_14_2013 == "O - Pension Credit"
            | benefits$hh57_20_2013 == "G - Universal Credit"
            | benefits$hh58_01_2013 ==
              "A - Incapacity Benefit (formerly Invalidity Benefit)"
            | benefits$hh58_02_2013 ==
              "B - Disability Living Allowance Care Component"
            | benefits$hh58_03_2013 ==
              "C - Disability Living Allowance Mobility Component"
            | benefits$hh58_06_2013 == "H - Severe Disablement benefit"
            | benefits$hh58_09_2013 ==
              "K - Disability premium with Income Support/Housing Benefit"
            | benefits$hh58_10_2013 == "L - Attendance allowance"
            | benefits$hh58_19_2013 ==
              "D - Personal Independence Payment Mobility Component"
            | benefits$hh58_20_2013 ==
              "E - Personal Independence Payment Daily Living Component"
            ,"TRUE"
            ,"FALSE")
  
  #Amalgamate the three years into one piece of information - if the occupant
  #recieved benefits in the year they were interviewed, hasoccupantsonbenefits is
  #set to "TRUE".
  benefits$benefit[benefits$benefit2011 == "TRUE" 
                   | benefits$benefit2012 == "TRUE" 
                   | benefits$benefit2013 == "TRUE"] <- "TRUE"
  benefits$benefit[benefits$benefit2011 == "FALSE" 
                   | benefits$benefit2012 == "FALSE" 
                   | benefits$benefit2013 == "FALSE"] <- "FALSE"
  
  return(benefits$benefit)
}
