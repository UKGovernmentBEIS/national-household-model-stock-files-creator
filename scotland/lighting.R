#' ---
#' title: "Lighting functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#+ echo=FALSE
#Sources (makes available) the common functions required to make the stock.
source("common.R", chdir=T)

#'##Make and save lighting data
#'
#'Create a .csv file using the function make.lighting, which creates a dataframe 
#'containing a complete set of populated variables for the lighting.csv stock file.
#'
#'The lighting dataframe contains only one variable - the fraction of lighting in the 
#' dwelling which is low energy lighting.
#'
#' @param shcs - the scottish survey data
#' 
#' @param output - the path to the output file including the required file name and
#' the extension .csv
save.lighting <- function(shcs, output) {
  write.csv(make.lighting(shcs), file=output, row.names=FALSE, na ="NULL")
}

#'Make the dataframe that contains all the information required for the lighting.csv
#' file
#'
#'The dataframe is constructed and returned (as it is the last thing in the function
#' that is assigned)
#' 
#'@param shcs - the scottish survey data
make.lighting <- function(shcs) {
  the.lightingfraction <- lighting.fraction(shcs$L18)
  
  data.frame(aacode = shcs$uprn_new,
             fraction = the.lightingfraction)
}

#'
#' \pagebreak
#' 
#' ##Create lighting fraction
#'  
#' Create the lighting fraction
#'  
#' @param lighting - column L18 from the SHCS. This is the tenths of lighting which 
#' is low energy lighting. This is divided by 10 to get a fraction (i.e. value 0-1.0)
#' For missing data, the low energy lighting is assummed to be 0.
lighting.fraction <- function(lighting){
  #Values of not applicable or unknown are turned into 0
  lighting <- checked.renum(lighting,
                            data.frame(a = c(88,99), b = c(0,0)))
  lighting <- lighting/10
}
