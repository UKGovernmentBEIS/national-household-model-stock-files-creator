#' ---
#' title: "Additional-properties functions"
#' author: "Centre for Sustainable Energy"
#' date: "2015"
#' output: pdf_document
#' ---

#'##Make additional-properties data
#'Create a .csv file using the function make.additionalproperties, which creates a
#'dataframe containing a complete set of populated variables for the additional-prope
#'rties.csv stock file.
#'
#'This file must be present in the stock import DTO package but can be empty save for
#'the aacode field containing the survey code. However, additional field can be added
#'to the file that can be called from scenarios in the NHM.
#'
#'Here a selection of variables of interest are selected in to a data.frame that is
#'saved as a csv file as part of the make.england function.
#'
#' @param allEntries - the merged collection of all SPSS files used in the stock
#' conversion process.


make.additionalproperties <- function(allEntries) {
                                        #creates a dataframe called additional.properties containing column names (word
                                        #before the = sign) and a vector to fill that column (vector after the = sign)
    additional.properties <- data.table(
        aacode = allEntries$aacode,
        gasmop = allEntries$gasmop,
        elecmop = allEntries$elecmop,
        housingcosts = allEntries$HousingCosts,
        AHCIncomeEQ = allEntries$AHCIncomeEQ,
        AHCeqFactor = allEntries$AHCeqFactor,
        BHCeqFactor = allEntries$BHCeqFactor,
        FuelCosteqFactor = allEntries$FuelCosteqFactor,
        fpLIHCflg = allEntries$fpLIHCflg,
        fpLIHCqdt = allEntries$fpLIHCqdt,
        fpLIHCgapEQ = allEntries$fpLIHCgapEQ,
        fpLIHCgapUNEQ = allEntries$fpLIHCgapUNEQ
    )
    print(paste("additional-properties DTO complete; number of records: ",
                nrow(additional.properties)))
    return(additional.properties)
}
