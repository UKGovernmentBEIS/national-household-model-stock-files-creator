#' ---
#' title: "Common functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'These functions are used by the majority of scottish scripts.
#'
#'#Relevel factors
#'
#'Wraps plyr::revalue to check that the levels in the revaluing
#' rule are exhaustive and complete.
#' 
#'@param a - vector to be releveled
#'
#'@param b - table consisting of old names matched to the required new names
#' 
checked.revalue <- function(a, b) {
    check.levels(a, names(b))
    plyr::revalue(a, b)
}

#'Utility to ensure that the levels of a given column are as required.
#' 
#'@param col - vector to be releveled (must be of type factor)
#'
#'@param check - list of old levels which have been matched to new names
check.levels <- function(col, check) {
  if (!is.factor(col)) {
    stop("column is not a factor")
  }
  l <- levels(col)
  if (!setequal(l, check)) {
    stop(cat(sep="\n",
             cat("Extra levels: ", setdiff(l, check), sep=","),
             cat("Missing levels: ", setdiff(check, l), sep=",")
    ))
  }
}

#'
#'\pagebreak
#'
#'#Relevel numbers
#'
#'Replaces values in y$a with those in y$b for numeric values
#' 
#'Used for replacing fields coded with numbers representing unobtainable or not
#' applicable with NA
#' 
#'@param x - vector to be releveled must be of type numeric
#'
#'@param y - dataframe with the original values in column a and the new values in
#' column b
checked.renum <- function(x,y) {
  if (!is.numeric(x)) {
    stop("column is not a numeric")
  }
  x <- mapvalues(x, from = y$a, to = y$b)
  return(x)
}


#'
#'\pagebreak
#'
#'#Building type and electric tariff
#'
#'Convert the dual column for building type into a single bit of data
#'
#'This function can be used replacing C2 with E7
#'
#'@param C1 - house type
#' 
#'@param C2 - flat type
building.type <- function(C1, C2) {
    as.factor(
        ifelse(
            C1 == "Not house",
            as.character(C2),
            as.character(C1)))
}

#'Flat test
#'
#'if the building type is one of four types indicating that it is a flat then it is
#' a flat
#'
#' @param building type - the type of building from the building.type function
is.flat <- function(buildingtype) {
    buildingtype %in% c("Tenement", "4-in-a-block"
                        , "Tower or slab", "Flat from conversion")
}
#'House test
#'
#'if the building is not a flat then it is a house
#'
#' @param building type - the type of building from the building.type function
is.house <- function(buildingtype) {
    !is.flat(buildingtype)
}


#'Electric tariff
#'
#'Sets the electric meter types to the correct format for the dto files
#' and ensures that stock with storage heating has an appropriate meter type
#' (i.e. not single)
#' 
#' @param meter a vector containing the electric meter type from the scottish survey
#' 
#' @param heatertype a vector contining the heater type from the scottish survey
electric.tariff <- function(meter,heatertype){
  meter <- as.factor(checked.revalue(
    meter,
    c("Single" = "flat_rate"
      ,"Dual" = "economy_7"
      ,"24 hour" = "twentyfour_hour_heating"
      ,"Not applicable" = "NULL"
      ,"Unobtainable" = "NULL"   
    )))
  meter[heatertype=="Storage heating" & meter == "flat_rate"] <- "economy_7"
  return(meter)
}


#' bespoke rounding function to ensure numbers are rounded correctly to the odd 
#' number not some stange international convention of only rounding to even numbers.
#' 
#' @param x - number to be rounded
#' 
#' @param n - number of decimal placed to round number too
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
